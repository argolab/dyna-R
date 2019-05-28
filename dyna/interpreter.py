from collections import defaultdict
from typing import *
import pprint


class RBaseType:

    __slots__ = ('_hashcache', '_constructed_from')

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
        return (self.__class__.__name__, *(c._tuple_rep for c in self.children))
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
        if type(self) is type(other) and len(self.vars) == len(other.vars) and len(self.children) == len(other.children):
            return all(a.possibly_equal(b) for a,b in zip(self.children,other.children))
        return False

    # def run_cb(self, frame, callback):
    #     frame, r = self.simplify(frame)
    #     if not r.isEmpty():
    #         return callback(frame, r)
    #     return frame, r

    def isEmpty(self):
        return False

    def all_vars(self):
        yield from self.vars
        for c in self.children:
            yield from c.all_vars()


    # def __add__(self, other):
    #     # in this case, it would be the union between these two expressions, wihch is not clear?
    #     if isinstance(other, Terminal):
    #         return other+self
    # def __mul__(self, other):
    #     return Intersect(self, other)

    def __bool__(self):
        raise RuntimeError('Should not check Rexpr with bool test, use None test')

class FinalState(RBaseType):
    __slots__ = ()
    pass

class Terminal(FinalState):
    __slots__ = ('multiplicity',)
    def __init__(self, multiplicity):
        super().__init__()
        self.multiplicity = multiplicity
    def __eq__(self, other):
        return type(self) is type(other) and self.multiplicity == other.multiplicity
    def __hash__(self):
        return hash(type(self)) ^ self.count
    def isEmpty(self):
        return self.multiplicity == 0


# if might be better to make this its own top level thing.  We might want to
# keep this around in the case that we can eventually determine that there is
# something else which can eleminate a branch
class _Error(FinalState):
    def isEmpty(self):
        # in this case, there is nothing here? so we can return that this is empty?
        # we need for the error states to be eleminated due to some other constraint, otherwise
        return True
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

class InvalidValue:
    pass
InvalidValue = InvalidValue()


class Variable:
    __slots__ = ()
    def __repr__(self):
        return f'var({str(self)})'

class VariableId(Variable):
    __slots__ = ('__name',)

    def __init__(self, name=None):
        if name is None:
            name = object()
        self.__name = name

    def isBound(self, frame):
        return self.__name in frame

    def getValue(self, frame):
        # want to ensure that we have some distinctive junk value so we are sure we do not use this
        # in C++ the junk would probably just be uninitalizied memory
        return frame.get(self.__name, InvalidValue)

    def setValue(self, frame, value):
        if self.__name in frame:
            # then check that the value is equal
            if frame[self.__name] != value:
                raise UnificationFailure()
        else:
            frame[self.__name] = value
        return True  # if not equal return values, todo handle this throughout the code

    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and (self.__name == other.__name))
    def __hash__(self):
        return hash(type(self)) ^ hash(self.__name)
    def __str__(self):
        return str(self.__name)

class ConstantVariable(Variable):
    __slots__ = ('__value',)
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
    return tuple((VariableId(v) if not isinstance(v, Variable) else v for v in vars))

ret_variable = VariableId('Return')

def constant(v):
    return ConstantVariable(None, v)

class Frame(dict):
    __slots__ = ()

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pprint.pformat(nice, indent=1)


class _EmptyFrame(Frame):
    __slots__ = ()
    def __setitem__(self, var, val):
        assert False  # don't set on this frame directly, can return a new instance that will
    # this is an empty dict, so bool(self) == False
    def update(self, *args, **kwargs):
        assert False

emptyFrame = _EmptyFrame()

class _FailedFrame(Frame):
    __slots__ = ()
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
# Iterators and other things

class Iterator:
    def bind_iterator(self, frame, variable, value):
        pass
    def run(self, frame):
        if 0:
            yield None
    @property
    def variables(self):
        # return the list of variables that will be bound by this iterator
        raise NotImplementedError()
    @property
    def consolidated(self):
        # we need to know if this is a consolidated iterator, so that we can determine if it is legal at this point in time
        raise NotImplementedError()


class UnionIterator(Iterator):
    def __init__(self, partition, variable, a, b):
        self.partition = partition
        self.variable = variable
        self.a = a
        self.b = b
    def bind_iterator(self, frame, variable, value):
        pass
    def run(self, frame):
        # this needs to identify the domain of the two iterators, and the
        # combine then such that it doesn't loop twice.  We are also going to
        # need to turn of branches of a partition when they are not productive.
        if 0:
            yield None
        assert False  # TODO


class RemapVarIterator(Iterator):
    def __init__(self, remap, wrapped):
        self.remap = remap
        self.wrapped = wrapped
    def run(self, frame):
        yield from self.wrapped.run(frame)
    def bind_iterator(self, frame, variable, value):
        assert False  # TODO





####################################################################################################
# Visitor and base definition for the core rewrites

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
        res = self.lookup(R)(R, *args, **kwargs)
        if R == res:
            return R
        # we want to track the R expr that this was constructed from as it might
        # still be useful for additional pattern matching against terms that
        # were already checked.
        #
        # for the case of simplify, we should also attempt to identify cases
        # where we are hitting the same state multiple times in which case we
        # may be able to compile those for the given mode that is being used
        res._constructed_from = R
        return res

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
    # these should be defined for all methods to do something
    raise NotImplementedError()

@simplify.define(Terminal)
def simplify_terminal(self, frame):
    return self



class PartitionVisitor(Visitor):
    def __call__(self, R, *args, **kwargs):
        yield from self.lookup(R)(R, *args, **kwargs)

getPartitions = PartitionVisitor()

@getPartitions.default
def getPartitions_default(self, frame):
    # go into the children by default, and see if they provide some way in which
    # they can be partitioned
    for c in self.children:
        yield from getPartitions(c, frame)


def runPartition(R, frame, partition):
    # this should yield different Frame, R pairs using the selected
    # partitionining scheme we use an iterator as we would like to be lazy, but
    # this iterator __must__ be finite, in that we could run
    # list(runPartition(...)) and it _would_ terminate with a fixed size list.

    # we might want to pattern match against the type of the partition in
    # different files?  So then this should also be a visitor pattern?  In which
    # case it would have to perform different rewrites of potentially nested
    # expressions.

    # running these partitions might also allow for there to be threading
    # between different operations?  In which case the consumer of this would
    # want to be able to run parallel for loop or something.

    assert False

    yield frame, R


# loop = Visitor()

# @loop.default
# def loop_default(self, frame, callback):
#     callback(self, frame)

# @loop.define(Terminal)
# def loop_terminal(self, frame, callback):
#     if self.multiplicity != 0:
#         callback(self, frame)

def loop_partition(R, frame, callback, partition):
    for bd in partition.run(frame):
        # make a copy of the frame for now would like to just modify the frame,
        # and track which variables are bound instead, so that this doesn't have
        # to make copies.  That is probably something that would become a
        # worthwhile optimization in the future.
        f = Frame(frame)
        try:
            for var, val in bd.items():  # we can't use update here as the names on variables are different from the values in the frame
                var.setValue(f, val)
            s = simplify(R, f)
            callback(s, f)
        except UnificationFailure:
            pass


def loop(R, frame, callback, partition=None):
    if partition is None:
        # then we need to select some partition to use, which will mean choosing
        # which one of theses is "best"?
        parts = getPartitions(R, frame)
        for p in parts:  # just choose something, and ensure that we can iterate this whole list without a problem
            partition = p

    assert isinstance(partition, Iterator)

    loop_partition(R, frame, callback, partition)


####################################################################################################
# the core R structure such as intersect and partition


class Intersect(RBaseType):

    def __init__(self, children :Tuple[RBaseType]):
        super().__init__()
        self._children = tuple(children)

    @property
    def children(self):
        return self._children

    def rewrite(self, rewriter):
        return intersect(*(rewriter(c) for c in self._children))

def intersect(*children):
    mul = 1
    r = []
    for c in children:
        if isinstance(c, Terminal):
            mul *= c.multiplicity
        else:
            r.append(c)
    if not r or mul == 0:
        return terminal(mul)
    if mul != 1:
        r.append(terminal(mul))
    if len(r) == 1:
        return r[0]
    return Intersect(tuple(r))


@simplify.define(Intersect)
def simplify_intersect(self :Intersect, frame: Frame):
    # TODO: this should handle early stoppin in the case that it gets a
    # multiplicity of zero.
    return intersect(*(simplify(c, frame) for c in self.children))


class Partition(RBaseType):
    """
    This class is /verhy/ overloaded in that we are going to be using the same representation for memoized entries as well as the partitions
    """
    def __init__(self, unioned_vars :Tuple, children :Tuple[Tuple[RBaseType, Tuple]]):
        super().__init__()
        self.unioned_vars = unioned_vars
        # the children should be considered immutable once placed on the partition class
        # though we are going to construct this class via

        # make the children simple in that we are just going to scan the list in the case that
        self.children = tuple(children)

    @property
    def vars(self):
        return self.unioned_vars
    @property
    def children(self):
        for v in self.children:
            yield v[0]

    def rewrite(self, rewriter):
        assert False  # TODO: loop over the children

def partition(unioned_vars, children):
    # construct a partition
    if all(isinstance(c, Terminal) for c in children):
        return Terminal(sum(c.multiplicity for c in children))

    return Partition(unioned_vars, tuple((c, (None,)*len(unioned_vars)) for c in children))


@simplify.define(Partition)
def simplify_partition(self :Partition, frame: Frame):
    var_vals = tuple(u.getValue(frame) for u in self.unioned_vars)
    def merge_tuples(a, b):
        for i,j in zip(a,b):
            if i!=j: raise 123  # something that indicates that these are not equal
            yield i or j  # return the one that is not null

    nc = defaultdict(list)
    assert False
    for k,v in self.children.items():
        # this needs to check that the assignment of variables is consistent otherwise skip it
        # then this needs to figure out what
        pass


@getPartitions.define(Partition)
def getPartitions_partition(self :Partition, frame):
    yield self
    assert False
    # TODO need to determine which variables we can also iterate, so this means
    # looking at the results from all of the children branches and then
    # filtering out things that are not going to work.  if variables are renamed
    # on the different branches, then it is possible that the iterators will
    # have to be able to handle those renamings.

    for p in self.children:
        pass


class Unify(RBaseType):
    def __init__(self, v1, v2):
        self.v1 = v1
        self.v2 = v2
    @property
    def vars(self):
        return (self.v1, self.v2)

@simplify.define(Unify)
def simplify_unify(self, frame):
    if self.v1.isBound(frame):
        v2.setValue(frame, self.v1.getValue(frame))
        return terminal(1)
    elif self.v2.isBound(frame):
        v1.setValue(frame, self.v2.getValue(frame))
        return terminal(1)
    return self


# lift and lower should probably be their own distinct operators in the code, so
# that it can do partial aggregation of an intermediate result.  This would be
# better for reusing an intermediate result.  the lift part needs to be done
# before memoization and aggregation takes place, whereas the lower part needs
# to happen after the result of the aggregation that combine the values
# together.  So they really shouldn't be referenced on this object.  Maybe only
# combine and combine_multiplicity.
class AggregatorOpBase:
    def lift(self, x): raise NotImplementedError()
    def lower(self, x): raise NotImplementedError()
    def combine(self, x, y): raise NotImplementedError()
    def combine_multiplicity(self, x, y, mul):
        # x + (y * mul)
        for _ in range(mul):
            x = self.combine(x, y)
        return x

class AggregatorOpImpl(AggregatorOpBase):
    def __init__(self, op): self.op = op
    def lift(self, x): return x
    def lower(self, x): return x
    def combine(self, x, y): return self.op(x,y)


class Aggregator(RBaseType):

    def __init__(self, result: Variable, head_vars: Tuple[Variable], bodyRes: Variable, aggregator :AggregatorOpBase, body :RBaseType):
        self.result = result
        self.bodyRes = bodyRes
        self.body = body
        self.head_vars = head_vars
        self.aggregator = aggregator
    @property
    def vars(self):
        return (self.result, self.bodyRes, *self.head_vars)
    @property
    def children(self):
        return (self.body,)


@simplify.define(Aggregator)
def simplify_aggregator(self, frame):
    # this needs to combine the results from multiple different partitions in the case that the head variables
    # are fully ground, otherwise, we are going to be unable to do anything
    # if we can run a deterministic operation then that should happen

    body = simplify(self.body, frame)
    if body.isEmpty():
        return body

    if all(v.isBound(frame) for v in self.head_vars):
        # In this case there should be something which is able to iterate the
        # different frames and their associated bodies.  If the bodies are not
        # fully grounded out, then we should attempt to handle that somehow?
        agg_result = None
        def loop_cb(R, frame):
            nonlocal agg_result
            # if this isn't a final state, then I suppose that we are going to
            # need to perform more loops?
            assert isinstance(R, FinalState)

            if agg_result is not None:
                agg_result = self.aggregator.combine(agg_result, self.bodyRes.getValue(frame))
            else:
                agg_result = self.bodyRes.getValue(frame)

        loop(body, frame, loop_cb)

        self.result.setValue(frame, agg_result)
        return terminal(1)  # return that we are done and the result of aggregation has been computed

    # There also needs to be some handling in the case that the result variable
    # from the body is fully grounded, but the head variables are not grounded.
    # (Meaning that this is something like `f(X) += 5.`)

    return Aggregator(self.result, self.head_vars, self.bodyRes, self.aggregator, body)