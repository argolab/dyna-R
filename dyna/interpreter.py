from collections import defaultdict
from typing import *
import pprint


class RBaseType:

    __slots__ = ('_hashcache', '_constructed_from')

    def __init__(self):
        self._hashcache = None
        self._constructed_from = None  # so that we can track what rewrites / transformations took place to get here

    @property
    def vars(self):
        return ()
    @property
    def children(self):
        return ()
    def _tuple_rep(self):
        return (self.__class__.__name__, *(c._tuple_rep() for c in self.children))
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

    def isEmpty(self):
        return False

    def all_vars(self):
        yield from self.vars
        for c in self.children:
            yield from c.all_vars()


    def __call__(self, *args, ret=None):
        if ret is None:
            ret = constant(True)  # just want the last variable to be true
        else:
            ret = variables_named(ret)[0]
        rm = {ret_variable: ret}
        args = variables_named(*args)
        rm.update(dict((VariableId(a), b) for a,b in zip(iter(int, 1), args)))

        # TODO: should this rename variables that are not referenced as
        # arguments?  In which case, this is going to be constructing new
        # variables (like what M. does)

        return self.rename_vars(lambda x: rm.get(x,x))

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
# _failure = Terminal(0)
# _done = Terminal(1)

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

    def _unset(self, frame):
        if self.__name in frame:
            del frame[self.__name]

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

class UnitaryVariable(Variable):
    # a variable that is not referenced in more than 1 place, so we are going to ignore the value, it isn't even a constant
    __slots__ = ()
    def __str__(self):
        return 'UNITARY'
    # __eq__ and __hash__ can just be the object version as we are not going to be equal to any other variable
    def isBound(self, frame):
        return False
    def getValue(self, frame):
        return InvalidValue
    def setValue(self, frame, value):
        assert value is not InvalidValue  # ignore setting a value as no one will read it.....
        return True

def variables_named(*vars):
    return tuple((VariableId(v) if not isinstance(v, Variable) else v for v in vars))

# R-exprs that are newely created, but not included will use 0,1,2,.... for the
# arguments and this variable as its return value.  We can then rewrite the
# expression such that the variables in an R-expr match the context they are
# being used in
ret_variable = VariableId('Return')

def constant(v):
    return ConstantVariable(None, v)

class Frame(dict):
    __slots__ = ()

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pprint.pformat(nice, indent=1)


# class _EmptyFrame(Frame):
#     __slots__ = ()
#     def __setitem__(self, var, val):
#         assert False  # don't set on this frame directly, can return a new instance that will
#     # this is an empty dict, so bool(self) == False
#     def update(self, *args, **kwargs):
#         assert False

# emptyFrame = _EmptyFrame()

# class _FailedFrame(Frame):
#     __slots__ = ()
#     def setVariable(self, variable, value):
#         return self
#     def isFailed(self):
#         return True
#     def remove(self, variable):
#         pass
#     def __setitem__(self, var, val):
#         assert False  # don't set values on the failed frame
#     def __repr__(self):
#         return '{FailedFrame, ...=...}'

# failedFrame = _FailedFrame()

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
    def __init__(self, partition, variable, iterators):
        self.partition = partition
        self.variable = variable
        self.iterators = iterators
    def bind_iterator(self, frame, variable, value):
        pass
    def run(self, frame):
        # this needs to identify the domain of the two iterators, and the
        # combine then such that it doesn't loop twice.  We are also going to
        # need to turn of branches of a partition when they are not productive.

        for i in range(len(self.iterators)):
            for val in self.iterators[i].run(frame):
                # check if any of the previous iterators produced this value
                vv = val[self.variable]
                emit = True
                for j in range(i):
                    # check the previous branches to see if they already emitted this value
                    if self.iterators[j].bind_iterator(frame, self.variable, vv):
                        emit = False
                        break
                if emit:
                    yield val


class RemapVarIterator(Iterator):
    def __init__(self, remap, wrapped):
        self.remap = remap
        self.wrapped = wrapped
    def run(self, frame):
        yield from self.wrapped.run(frame)
    def bind_iterator(self, frame, variable, value):
        assert False  # TODO


class SingleIterator(Iterator):
    # iterator over a single constant value
    def __init__(self, variable, value):
        self.variable = variable
        self.value = value
    def run(self, frame):
        yield {self.variable: self.value}
    def bind_iterator(self, frame, variable, value):
        assert self.variable == variable
        assert self.value == value
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


def saturate(R, frame):
    while True:
        # the frame is getting modified and the R is returning potentially new
        # things.  by saturating this, we are going to run until there is
        # nothing that we are able to do.
        last_R = R
        R = simplify(R, frame)
        if R == last_R:
            break
    return R, frame


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
        self._unioned_vars = unioned_vars
        # the children should be considered immutable once placed on the partition class
        # though we are going to construct this class via

        # make the children simple in that we are just going to scan the list in the case that
        self._children = tuple(children)

    @property
    def vars(self):
        return self._unioned_vars
    @property
    def children(self):
        for v in self._children:
            yield v[1]

    def rewrite(self, rewriter):
        assert False  # TODO: loop over the children

def partition(unioned_vars, children):
    # construct a partition
    if all(isinstance(c, Terminal) for c in children):
        return Terminal(sum(c.multiplicity for c in children))

    return Partition(unioned_vars, tuple(((None,)*len(unioned_vars), c) for c in children))


@simplify.define(Partition)
def simplify_partition(self :Partition, frame: Frame):
    incoming_mode = [v.isBound(frame) for v in self._unioned_vars]
    incoming_values = [v.getValue(frame) for v in self._unioned_vars]

    nc = defaultdict(list)

    # depending on the storage strategy of this, going to need to have something
    # better?  This is going to require that
    for grounds, Rexpr in self._children:
        # this needs to check that the assignment of variables is consistent otherwise skip it
        # then this needs to figure out what

        # first determine if this element matches the values
        should_run = all((not a or c is None or b == c) for a,b,c in zip(incoming_mode, incoming_values, grounds))

        if should_run:
            # in the case that an incoming argument is already bound then we are going to want to record this under that new operation
            # we are first going to need to set the value of any variable that should already be bound

            for var, val in zip(self._unioned_vars, grounds):
                if val is not None:
                    var.setValue(frame, val)

            res = simplify(Rexpr, frame)
            # this would have bound new values in the frame potentially, so we going to unset those (if they were unset on being called)
            nkey = tuple(v.getValue(frame) if v.isBound(frame) else None for v in self._unioned_vars)
            for var, imode in zip(self._unioned_vars, incoming_mode):
                if not imode:
                    var._unset(frame)  # TODO: figure out a better way to do this in python

            nc[nkey].append(res)

    ll = []
    for k, vv in nc.items():  # the partition should probably have more of a
                              # dict structure, this needs to match what the
                              # internal data structure is for partition
        for v in vv:
            ll.append((k, v))

    if all(isinstance(l[1], Terminal) for l in ll):
        return Terminal(sum(l[1].multiplicity for l in ll))

    return Partition(self._unioned_vars, ll)





@getPartitions.define(Partition)
def getPartitions_partition(self :Partition, frame):
    # TODO need to determine which variables we can also iterate, so this means
    # looking at the results from all of the children branches and then
    # filtering out things that are not going to work.  if variables are renamed
    # on the different branches, then it is possible that the iterators will
    # have to be able to handle those renamings.

    # this needs to determine which partitions in the children might be useful.
    # If there are variables that can be iterated, then we are going to have to
    # construct the union iterators.  If there are further nested partitions,
    # then we are also going to have to determine those methods

    # if we are collecting different partition methods from the children
    # partitions, then there can be a lot of different cases?  If there are lots of branches, then that could become problematic

    # map of all of the children branches
    #vmap = [[None]*len(self._unioned_vars) for _ in range(len(self._children))]

    citers = []
    incoming_mode = [v.isBound(frame) for v in self._unioned_vars]

    vmap = {v: i for i, v in enumerate(self._unioned_vars)}

    vmaps = []

    for vals, child in self._children:
        # need to get all of the iterators from this child branch, in the case
        # that a variable is already bound want to include that.  If a variable
        # is not in the union map, then we want to ignore it

        vm = [None]*len(self._unioned_vars)
        for i, val in enumerate(vals):
            if val is not None:
                vm[i] = SingleIterator(self._unioned_vars[i], val)

        for it in getPartitions(child, frame):
            if isinstance(it, Partition):  # if this is not consolidated, then we are going to want to bind a variable
               citers.append(it)  # these are just partitions, so buffer these I suppose
            else:
                # then this is going to be some variable that
                if it.variable in vmap:
                    if vm[vmap[it.variable]] is None:  # this should potentially take more than 1 iterator rather than the first
                        vm[vmap[it.variable]] = it
        vmaps.append(vm)

    # we are going to have to union all of the different branches of a variable
    # and if possible, we are going to indicate that we can iterate this variable
    for i, var in enumerate(self._unioned_vars):
        if incoming_mode[i]:
            continue
        vs = [v[i] for v in vmaps]
        if all(v is not None for v in vs):
            # then we can iterate this variable

            yield UnionIterator(self, var, vs)

    # yield any partitions which can be branched (after the unions over variables

    # there needs to be some partition iterator, which loops over the branches
    # which are potentially not non-overlapping.  In which case we are going to
    # want to collect those as more nested partitions.  I suppose that this can
    # handle if there are


    # yield self
    # yield from citers


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
