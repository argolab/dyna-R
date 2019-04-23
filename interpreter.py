from collections import defaultdict
from typing import *


class RBaseType:
    def simplify(self, frame):
        raise NotImplementedError()

    def get_var_domains(self, frame):
        ret = defaultdict(set)
        self._get_iterators(frame, ret)
        return ret
    def _get_iterators(self, frame, ret):
        for c in self.children:
            c._get_iterators(frame, ret)

    def get_partitions(self, frame):
        # return branches over union objects this recurse to the children?
        # though we don't necessairly want to walk into the union itself, we
        # need to choose at the top level first?
        #
        # if there is a union, then we don't necessairly need to fully run the
        # union totally, just need to group and branch the branches, which would
        # be interesting?
        for c in self.children:
            yield from c.get_partitions(frame)

    @property
    def vars(self):
        return ()
    @property
    def children(self):
        return ()
    def disp(self, indent):
        n = self.__class__.__name__
        return f'{indent}{n}('+', '.join(map(str, self.vars)) + ',\n' + ''.join(c.disp(indent + ' '*(len(n) + 1)) for c in self.children) + ')'
    def __repr__(self):
        return self.disp('')
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.children == other.children and
                                   self.vars == other.vars)
    def __hash__(self):
        hv = getattr(self, '_hashcache', None)
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

# class Ffunction(FBaseType):
#     def __init__(self, func):
#         super().__init__()
#         self.func = func
#     def simplify(self, frame):
#         return self.func(frame)
#     def __eq__(self, other):
#         return type(self) is type(other) and self.func is other.func
#     def __hash__(self):
#         return hash(self.func)
#     def disp(self, indent):
#         ret = [f'{indent}{self.func.__name__}(\n']
#         if self.func.__closure__:
#             for c in self.func.__closure__:
#                 if isinstance(c.cell_contents, FBaseType) and c.cell_contents is not self:
#                     ret.append(c.cell_contents.disp(' ' * len(ret[0])) + ',')
#         ret[-1] = ret[-1][:-1] + ')'
#         return ''.join(ret)


class Terminal(RBaseType):
    def __init__(self, multiplicity):
        super().__init__()
        self.multiplicity = multiplicity
    def simplify(self, frame):
        return self
    def __eq__(self, other):
        return type(self) is type(other) and self.multiplicity == other.multiplicity
    def __hash__(self):
        return hash(type(self)) ^ self.count
    def disp(self, indent):
        return '{indent}Terminal({self.count})'
    def isEmpty(self):
        return self.multiplicity == 0


class _Error(Terminal):
    def __init__(self):
        super.__init__(0)
error = _Error()



####################################################################################################
# Frame base type


class ConstantVariable:
    def __init__(self, var, value):
        self.value = value
    # I suppose that if we have two variables that take on the same value, even if they weren't unified together
    # we /could/ consider them the same variable?  There isn't that much of a difference in this case
    def __str__(self):
        return f'={self.value}'
    def __repr__(self):
        return str(self)
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and self.value == other.value)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.value)

class Frame(dict):
    def setVariable(self, variable, value):
        assert not isinstance(value, Iterator)
        if isinstance(variable, ConstantVariable):
            if variable.value == value:
                return self
            else:
                return failedFrame
        elif variable in self:
            if self[variable] == value:
                return self
            else:
                return failedFrame
        else:
            f = Frame(self)
            f[variable] = value
            return f

    def getVariable(self, variable):
        if isinstance(variable, ConstantVariable):
            return variable.value
        return self.get(variable, None)

    # short cut for this particular operation
    def isBound(self, variable):
        return isinstance(variable, ConstantVariable) or variable in self

    # need to clear the slot, or maybe we are just trying to be tidy in this implementation
    def remove(self, variable):
        self.pop(variable, None)

    def isFailed(self):
        return False

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pformat(nice, indent=1)

    def merge(self, other):
        if not other:
            return self
        if not self:
            return other
        r = Frame(self)
        r.update(other)
        return r

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
        return self._methods.get(type(item), self._default)(R, *args, **kwargs)


simplify = Visitor()

@simplify.default
def simplify_default(self):
    # then there is nothing that we are going to be able to do in the rewrites
    return self


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
