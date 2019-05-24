from collections import defaultdict
from typing import *
import pprint


class RBaseType:

    def __init__(self):
        self._hashcache = None
        self._constructed_from = None  # so that we can track what rewrites / transformations took place to get here

    # def get_partitions(self, frame):
    #     for c in self.children:
    #         yield from c.get_partitions(frame)

    @property
    def vars(self):
        return ()
    @property
    def children(self):
        return ()
    def _tuple_rep(self):
        return (self.__class__.__name__, *(c._tuple_rep for c in self.children()))
    def __repr__(self):
        return pprint.pformat(self._tuple_rep())
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.children == other.children and
                                   self.vars == other.vars)
    def __hash__(self):
        hv = self._hashcache
        if hv is not None:
            return hv
        hv = (hash(self.__class__) ^
              hash(self.children) ^
              hash(self.vars))
        self._hashcache = hv
        return hv

    def rewrite(self, rewriter=lambda x: x):
        return self
    def rename_vars(self, remap):
        return self.rewrite(lambda c: c.rename_vars(remap))
    def possibly_equal(self, other):
        # for help aligning constraints during optimization to perform rewriting
        # should consider everything except variables names
        return type(self) is type(other) and len(self.vars) == len(other.vars)

    def run_cb(self, frame, callback):
        frame, r = self.simplify(frame)
        if not r.isEmpty():
            return callback(frame, r)
        return frame, r

    def isEmpty(self):
        return False

class Terminal(RBaseType):
    def __init__(self, multiplicity):
        super().__init__()
        self.multiplicity = multiplicity
    def __eq__(self, other):
        return type(self) is type(other) and self.multiplicity == other.multiplicity
    def __hash__(self):
        return hash(type(self)) ^ self.count
    def isEmpty(self):
        return self.multiplicity == 0


class _Error(Terminal):
    def __init__(self):
        super.__init__(0)
error = _Error()

# do not duplicate these as much as possible
_failure = Terminal(0)
_done = Terminal(1)

def terminal(n):
    # if n == 0:
    #     return _failure
    # elif n == 1:
    #     return _done
    # elif return Terminal(n)
    return Terminal(n)


####################################################################################################
# Frame base type


class UnificationFailure(Exception):
    # throw this in the case that setting the variable on the frame fails.
    # using this will simplify the implementation in that we don't have to check
    # as much stuff (hopefully) though, we are going to handle the results of
    # failures.
    pass


class VariableBase:
    def __repr__(self):
        return str(self)

class VariableId(VariableBase):

    def __init__(self, name):
        self.__name = name

    def isBound(self, frame):
        return self.__name in frame

    def getValue(self, frame):
        return frame.get(self.__name)

    def setValue(self, frame, value):
        if self.__name in frame:
            # then check that the value is equal
            if frame[self.__name] != value:
                raise UnificationFailure()
        else:
            frame[self.__name] = value
        return True  # if not equal return values, todo handle this throughout the code

    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and self.__name == other.__name)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.__name)
    def __str__(self):
        return str(self.__name)

class ConstantVariable:
    def __init__(self, var, value):
        self.__value = value
    # I suppose that if we have two variables that take on the same value, even if they weren't unified together
    # we /could/ consider them the same variable?  There isn't that much of a difference in this case
    def __str__(self):
        return f'={self.__value}'
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and self.value == other.value)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.value)

    def isBound(self, frame):
        return True
    def getValue(self, frame):
        return self.__value

    def setValue(self, frame, value):
        if value != self.__value:  # otherwise we are going to have to make the result terminal with a value of zero
            raise UnificationFailure()
        return True

def variables_named(*vars):
    return tuple((VariableId(v) for v in vars))

class Frame(dict):

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pformat(nice, indent=1)

# def setVariable(frame: Frame, variable, value):
#     pass

# def getVariable(frame: Frame, variable):
#     if isinstance(variable, ConstantVariable):
#         return variable.value
#     return frame.get(variable, None)

# def isBound(frame: Frame, variable):
#     return isinstance(variable, ConstantVariable) or variable in frame


class _EmptyFrame(Frame):
    def __setitem__(self, var, val):
        assert False  # don't set on this frame directly, can return a new instance that will
    # this is an empty dict, so bool(self) == False
    def update(self, *args, **kwargs):
        assert False

emptyFrame = _EmptyFrame()

class _FailedFrame(Frame):
    def setVariable(self, variable, value):
        return self
    def isFailed(self):
        return True
    def remove(self, variable):
        pass
    def __setitem__(self, var, val):
        assert False  # don't set values on the failed frame
    def __repr__(self):
        return '{FailedFrame, ...=...}'

failedFrame = _FailedFrame()


####################################################################################################

class Visitor:
    def __init__(self):
        self._methods = {}
        self._default = lambda *args: args[0]
    def define(self, typ):
        def f(method):
            self._methods[typ] = method
            return method
        return f
    def default(self, method):
        self._default = method
    def delay(self, typ):
        # this should basically be used for external calls that this rewrite can
        # not see into.  If these are delayed rewrites, then we are going to
        # want to get all of the referenced expressions.
        # if there is some rewrite that is being applied to an operation, then we can
        raise NotImplementedError()

    def __call__(self, R :RBaseType, *args, **kwargs):
        return self.lookup(R)(R, *args, **kwargs)

    def lookup(self, R):
        return self._methods.get(type(R), self._default)



class SimplifyVisitor(Visitor):
    def __call__(self, R, *args, **kwargs):
        # special handling for unification failure though, maybe this should
        # just be handled in the unions?  Everything else should just end up
        # pushing this failure up the chain?  Though maybe that is closer to
        # what we want
        try:
            return super().__call__(R, *args, **kwargs)
        except UnificationFailure:
            return terminal(0)

simplify = SimplifyVisitor()

@simplify.default
def simplify_default(self, frame):
    # then there is nothing that we are going to be able to do in the rewrites
    assert False  # not defined
    #return self





getPartitions = Visitor()

@getPartitions.default
def getPartitions_default(self):
    # go into the children by default, and see if they provide some way in which
    # they can be partitioned
    for c in self.children:
        yield from getPartitions(c)


def runPartition(Frame, R, partition):
    # this should yield different Frame, R pairs using the selected
    # partitionining scheme we use an iterator as we would like to be lazy, but
    # this iterator __must__ be finite, in that we could run
    # list(runPartition(...)) and it _would_ terminate with a fixed size list.


    yield Frame, R
