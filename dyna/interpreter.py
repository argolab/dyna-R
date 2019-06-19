from collections import defaultdict
from typing import *
import pprint

import networkx as nx

from .prefix_trie import PrefixTrie


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

    def all_children(self):
        yield self
        for c in self.children:
            yield from c.all_children()

    def rename_vars_unique(self, nmap):
        # rename variables where new names are generated for variables that are not referenced
        nvars = {}
        def rmap(x):
            nonlocal nvars
            if isinstance(x, UnitaryVariable) or isinstance(x, ConstantVariable):
                return x
            if x in nvars:
                return nvars[x]
            r = nmap(x)
            if r is None:
                r = VariableId()  # make a new unique variable id
            nvars[x] = r
            return r
        return self.rename_vars(rmap)

    def weak_equiv(self, ignored=()):
        # try and make the expressions the same by renaming variables in a
        # consistent way.  ideally, we can pattern match against these
        # expressions more easily later?
        vs = set(ignored)  # start with variables that we do not want to normalize the names
        vl = []
        for var in self.all_vars():
            if not isinstance(var, ConstantVariable) and var not in vs:
                vl.append(var)
        rm = dict(zip(vl, (VariableId(f'$W{i}') for i in range(len(vl)))))
        return self.rename_vars(lambda x: rm.get(x,x)), dict((v,k) for k,v in rm.items())


    def __call__(self, *args, ret=None):
        # TODO: this needs to check that we are not calling this with more arguments than are present in the code
        # otherwise that will just be a subtle bug that is hard to detect..
        if ret is None:
            ret = constant(True)  # just want the last variable to be true
        else:
            ret = variables_named(ret)[0]
        rm = {ret_variable: ret}
        args = variables_named(*args)
        rm.update(dict((VariableId(a), b) for a,b in enumerate(args)))

        return self.rename_vars_unique(rm.get)

    def __bool__(self):
        raise RuntimeError('Should not check Rexpr with bool test, use None test')

    def __lt__(self, other):
        if not isinstance(other, type(self)):
            return self.__class__.__name__ < other.__class__.__name__
        return self._tuple_rep() < other._tuple_rep()

    # def build_graph(self, graph):
    #     for v in self.vars:
    #         v.build_graph(graph)

    # def isomorphic(self, other, matched_variables=()):
    #     # determine if the two graphs are isomprhic to eachother, but we are going to require that


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
        return hash(type(self)) ^ self.multiplicity
    def isEmpty(self):
        return self.multiplicity == 0
    def _tuple_rep(self):
        return (self.__class__.__name__, self.multiplicity)


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
    def __str__(self):
        return 'INVALID'
    def __repr__(self):
        return 'INVALID'
InvalidValue = InvalidValue()



class Variable:
    __slots__ = ()
    def __repr__(self):
        return f'var({str(self)})'
    def __lt__(self, other):
        # just something so that we can order these and select something consistently
        return str(self) < str(other)

annon_variable_name = 0
class VariableId(Variable):
    __slots__ = ('__name',)

    def __init__(self, name=None):
        global annon_variable_name
        if name is None:
            name = f'$V{annon_variable_name}'
            annon_variable_name += 1
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
        return (self is other) or (type(self) is type(other) and self.__value == other.__value)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.__value)

    def isBound(self, frame):
        return True
    def getValue(self, frame):
        return self.__value

    def setValue(self, frame, value):
        if value != self.__value:  # otherwise we are going to have to make the result terminal with a value of zero
            raise UnificationFailure()
        return True

    def __lt__(self, other):
        assert isinstance(other, Variable)
        if isinstance(other, ConstantVariable):
            return self.__value < other.__value
        else:
            return True

class UnitaryVariable(Variable):
    # a variable that is not referenced in more than 1 place, so we are going to
    # ignore the value, it isn't even a constant

    # note, this is different from `_` in the source language, as that has to be
    # "referenced" in two places so that it is looped over
    __slots__ = ()
    def __init__(self):
        assert False  # Dont use this
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
    __slots__ = ('call_stack', 'memo_reads', 'assumption_tracker')

    def __init__(self, f=None):
        if f is not None:
            super().__init__(f)
            self.call_stack = f.call_stack.copy()
            self.memo_reads = f.memo_reads
            self.assumption_tracker = f.assumption_tracker
        else:
            super().__init__()
            self.call_stack = []
            self.memo_reads = True  # if the memo tables are allowed to perform reads (making the results dependent on the values saved, otherwise just don't run)
            self.assumption_tracker = lambda x: None  # when we encounter an assumption during simplification, log that here

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pprint.pformat(nice, indent=1)



####################################################################################################
# Iterators and other things

class Iterator:
    def bind_iterator(self, frame, variable, value):
        raise NotImplementedError()
    def run(self, frame):
        raise NotImplementedError()

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
        assert variable == self.variable
        return any(v.bind_iterator(self.variable, value) for v in self.iterators)
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
    def __init__(self, remap, wrapped, variable):
        self.remap = remap
        self.wrapped = wrapped
        self.variable = variable
    def run(self, frame):
        for r in self.wrapped.run(None):  # TODO: handle the remapping of the argument frame

            yield {self.remap[k]: v for k,v in r.items()}

            # # TODO: handle remapping the keys
            # assert False
            # yield 0

    def bind_iterator(self, frame, variable, value):
        rmap = dict((b,a) for a,b in self.remap.items())
        v = rmap.get(variable, variable)
        return self.wrapped.bind_iterator(self, None, r, value)




class SingleIterator(Iterator):
    # iterator over a single constant value
    def __init__(self, variable, value):
        self.variable = variable
        self.value = value
    def run(self, frame):
        yield {self.variable: self.value}
    def bind_iterator(self, frame, variable, value):
        assert self.variable == variable
        return self.value == value  # return if this iterator would have emitted this value


####################################################################################################
# Visitor and base definition for the core rewrites

class Visitor:
    def __init__(self, track_source=True):
        self._methods = {}
        self._default = lambda R, *args, **kwargs: R.rewrite(lambda x: self(x, *args, **kwargs))
        self._track_source = track_source
    def define(self, typ):
        def f(method):
            self._methods[typ] = method
            return method
        return f
    def default(self, method):
        self._default = method

    # def delay(self, typ):
    #     # this should basically be used for external calls that this rewrite can
    #     # not see into.  If these are delayed rewrites, then we are going to
    #     # want to get all of the referenced expressions.
    #     # if there is some rewrite that is being applied to an operation, then we can
    #     raise NotImplementedError()

    def __call__(self, R :RBaseType, *args, **kwargs):
        res = self.lookup(R)(R, *args, **kwargs)
        if not self._track_source:
            return res
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


class RefinedVisitor(Visitor):
    def __init__(self, track_source=True):
        super().__init__(track_source)
        self._refined_methods = {}
    def define_refined(self, refined):
        # for things like moded op, there are lots of expressions which are
        # under the same operator, but we might want to match against specific
        # patterns.  So we attempt to normalize the varible names and then use
        # that as an equality matching on these expressions
        assert isinstance(refined, RBaseType)
        rf, _ = refined.weak_equiv()

        def f(method):
            self._refined_methods.setdefault(type(rf), {})[rf] = method
            return method
        return f
    def lookup(self, R):
        # don't make this virtual in C++....
        typ = type(R)
        if self._refined_methods:
            z = self._refined_methods.get(typ)
            if z is not None:
                rf = z.get(R.weak_equiv()[0])
                if rf is not None:
                    return rf
        return self._methods.get(typ, self._default)


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


def saturate(R, frame, *, log=False):
    while True:
        # the frame is getting modified and the R is returning potentially new
        # things.  by saturating this, we are going to run until there is
        # nothing that we are able to do.
        if log:
            print(R)
            print(frame)
            print('-'*50)
        last_R = R
        R = simplify(R, frame)
        if R == last_R:
            break
        #import ipdb; ipdb.set_trace()
    return R


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


def loop_partition(R, frame, callback, partition):
    # use a callback instead of iterator as will be easier to rewrite this later
    for bd in partition.run(frame):
        # make a copy of the frame for now would like to just modify the frame,
        # and track which variables are bound instead, so that this doesn't have
        # to make copies.  That is probably something that would become a
        # worthwhile optimization in the future.
        f = Frame(frame)
        try:
            for var, val in bd.items():  # we can't use update here as the names on variables are different from the values in the frame
                var.setValue(f, val)
            s = saturate(R, f)  # probably want this to be handled via the callback?
            callback(s, f)
        except UnificationFailure:
            pass


def loop(R, frame, callback, till_terminal=False, best_effort=False, partition=None):
    # there should really be some parameter like "effort" which can range between best, quick, till_terminal etc.  and these can error out in different ways


    if isinstance(R, FinalState):
        # then this is done, so just callback
        callback(R, frame)
        return

    if partition is None:
        # then we need to select some partition to use, which will mean choosing
        # which one of theses is "best"?
        parts = getPartitions(R, frame)
        for p in parts:  # just choose something, and ensure that we can iterate this whole list without a problem
            partition = p

    if partition is None:
        # try 2
        R = make_aggregator_loopable(R, frame=frame)
        parts = getPartitions(R, frame)
        for p in parts:  # just choose something, and ensure that we can iterate this whole list without a problem
            partition = p

    if not best_effort:
        assert isinstance(partition, Iterator)
    else:
        if partition is None:
            # then we couldn't find something to iterate, so we are going to just callback
            callback(R, frame)
            return

    if till_terminal or best_effort:
        def cb(r, f):
            if isinstance(r, FinalState):
                callback(r, f)
            else:
                loop(r, f, callback, till_terminal=till_terminal, best_effort=best_effort)
    else:
        cb = callback

    loop_partition(R, frame, cb, partition)


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
    for cc in children:
        if isinstance(cc, Intersect):
            cc = cc._children
        else:
            cc = [cc]
        for c in cc:  # flatten out the intersects when we can
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
    # TODO: this should handle early stopping in the case that it gets a
    # multiplicity of zero.
    vs = []
    for c in self.children:
        r = simplify(c, frame)
        if r.isEmpty():
            return r
        vs.append(r)
    return intersect(*vs)


class Partition(RBaseType):
    """
    This class is /verhy/ overloaded in that we are going to be using the same representation for memoized entries as well as the partitions
    """
    def __init__(self, unioned_vars :Tuple, children :PrefixTrie):#Dict[Tuple[object], List[RBaseType]]):
        super().__init__()
        self._unioned_vars = unioned_vars
        # the children should be considered immutable once placed on the partition class
        # though we are going to construct this class via

        assert isinstance(children, PrefixTrie)

        # make the children simple in that we are just going to scan the list in the case that
        self._children = children  # this should be treated as "immutable"

    @property
    def vars(self):
        return self._unioned_vars
    @property
    def children(self):
        return tuple(c for v in self._children.values() for c in v)

    def rename_vars(self, remap):
        r = tuple(remap(u) for u in self._unioned_vars)
        #c = dict((k, [c.rename_vars(remap) for c in v]) for k, v in self._children.items())
        c = self._children.map_values(lambda v: [a.rename_vars(remap) for a in v])
        return Partition(r, c)

    def rewrite(self, rewriter):
        #c = dict((k, [rewriter(c) for c in v]) for k,v in self._children.items())
        c = self._children.map_values(lambda v: [rewriter(a) for a in v])
        return Partition(self._unioned_vars, c)

    def _tuple_rep(self):
        # though might want to have the representation of what the values are on each branch of the partition
        return (self.__class__.__name__, self._unioned_vars, *((k, [v._tuple_rep() for v in vv]) for k,vv in self._children.items()))

    def __eq__(self, other):
        # the equal bit needs to include the arguments on the heads of the children expressions
        return self is other or \
            (type(self) is type(other) and
             self._unioned_vars == other._unioned_vars and
             # the order of the children can still impact this test, which is ...bad...sigh
             self._children == other._children)

             # len(self._children) == len(other._children) and
             # # TODO: this needs to be handled as a set or something rather than just looping over the lists
             # all(a == b for a,b in zip(self._children, other._children)))

    def __hash__(self):
        # anytime that the children are updated, the hash would change, which is
        # something that sorta breaks the idea of hashing this object or the immutablility
        return super().__hash__()


def partition(unioned_vars, children):
    # construct a partition
    if all(isinstance(c, Terminal) for c in children):
        return Terminal(sum(c.multiplicity for c in children))

    c = PrefixTrie(len(unioned_vars))
    c[(None,)*len(unioned_vars)] = list(children)

    return Partition(unioned_vars, c)


@simplify.define(Partition)
def simplify_partition(self :Partition, frame: Frame, *, map_function=None, reduce_to_single=True, simplify_rexprs=True, flatten_keys=False):  # TODO: better name than map_function???
    """This is the simplify method for partition, but there is so many special
    arguments which lets its behavior be modified, that it deserves some special
    attention


    map_function is called before anything is saved back into the partition.  This lets there be some special handling and modification of the values before saving
    reduce_to_single controls if the partition will delete itself in the case that it is not needed.  Some operations like memos require that they are contained in a partition even if there is one expression
    simplify_rexpr is if we should call simplify ourselves before trying to save it back
    flatten_keys will use loop to try and make this as close as possible to a fully ground table.  There might still be some non-ground R-exprs that are remaining in the partition.  This is essentially a temp memo used during computation and then thrown away

    """

    incoming_mode = [v.isBound(frame) for v in self._unioned_vars]
    incoming_values = [v.getValue(frame) for v in self._unioned_vars]

    #nc = defaultdict(list)
    nc = PrefixTrie(len(self._unioned_vars))

    def saveL(res, frame):
        # this would have bound new values in the frame potentially, so we going to unset those (if they were unset on being called)
        nkey = tuple(v.getValue(frame) if v.isBound(frame) else None for v in self._unioned_vars)

        if not res.isEmpty():
            #nc[nkey].append(res)
            nc.setdefault(nkey,[]).append(res)

    #saveL.unioned_vars = self._unioned_vars

    # a hook so that we can handle perform some remapping and control what gets save back into the partition
    if flatten_keys:
        assert map_function is None  # currently don't allow double here
        def save(res2, frame2):
            if isinstance(res2, FinalState):
                saveL(res2, frame2)
            else:
                # then we have to try and loop this to ground out the values
                def cb(res3, frame3):
                    res4  = res3.rename_vars_unique(lambda x: constant(x.getValue(frame3)) if x.isBound(frame3) else (x if x in self._unioned_vars else None))
                    saveL(res4, frame3)
                loop(res2, frame2, cb, best_effort=True)

    elif map_function is None:
        save = saveL
    else:
        save = lambda a,b: map_function(saveL, a,b)


    # depending on the storage strategy of this, going to need to have something
    # better?  This is going to require that

    for grounds, Rexprs in self._children.filter([a if b else None for a,b in zip(incoming_values, incoming_mode)]):

        #for grounds, Rexprs in self._children.items():
        # this needs to check that the assignment of variables is consistent otherwise skip it
        # then this needs to figure out what

        # first determine if this element matches the values
        should_run = all((not a or c is None or b == c) for a,b,c in zip(incoming_mode, incoming_values, grounds))

        assert should_run

        #if should_run:
            # in the case that an incoming argument is already bound then we are going to want to record this under that new operation
            # we are first going to need to set the value of any variable that should already be bound

        for var, val in zip(self._unioned_vars, grounds):
            if val is not None:
                var.setValue(frame, val)

        for Rexpr in Rexprs:
            if simplify_rexprs:
                res = simplify(Rexpr, frame)
            else:
                res = Rexpr
            save(res, frame)

            for var, imode, val in zip(self._unioned_vars, incoming_mode, grounds):
                if not imode and val is None:
                    var._unset(frame)

        for var, imode in zip(self._unioned_vars, incoming_mode):
            if not imode:
                var._unset(frame)

    if not nc:
        # then nothing matched, so just return that the partition is empty
        return Terminal(0)

    # if 'ddd' in frame:
    #     import ipdb; ipdb.set_trace()

    set_values = list(next(iter(nc.keys())))

    for k, vv in nc:

        # identify which values are the same across all branches
        for i in range(len(self._unioned_vars)):
            if set_values[i] != k[i]:
                set_values[i] = None

        #vv = nc[k]
        multiplicity = 0
        r = []
        for v in vv:
            if isinstance(v, Terminal):
                multiplicity += v.multiplicity
                assert None not in k
            else:
                r.append(v)
        if multiplicity != 0:
            r.append(Terminal(multiplicity))
        nc[k] = r

    for var, val in zip(self._unioned_vars, set_values):
        if val is not None:
            try:
                var.setValue(frame, val)
            except UnificationFailure:
                assert False # ??? shouldn't happen

    if reduce_to_single:
        r = nc.single_item()
        if r is not None and len(r[1]) == 1:
            return r[1][0]

    return Partition(self._unioned_vars, nc)


def partition_lookup(self :Partition, key):
    # return a new partition that matches the values that are in the key
    assert len(self._unioned_vars) == len(key)

    nc = self._children.filter(key)
    if not nc:
        return None
    return Partition(self._unioned_vars, nc)

    # res = {}
    # for grounds, Rexprs in self._children.items():
    #     # determine if this matches
    #     matches = all(a is None or b is None or a == b for a,b in zip(key, grounds))
    #     if matches:
    #         res[grounds] = Rexprs

    # if res:
    #     return Partition(self._unioned_vars, res)
    # # if there is nothing that matched, then return None which could represent a "needs to be computed" or terminal(0) based off context
    # return None


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

    for vals, Rexprs in self._children.items():
        # need to get all of the iterators from this child branch, in the case
        # that a variable is already bound want to include that.  If a variable
        # is not in the union map, then we want to ignore it

        vm_ = [None]*len(self._unioned_vars)
        for i, val in enumerate(vals):
            if val is not None:
                vm_[i] = SingleIterator(self._unioned_vars[i], val)

        for child in Rexprs:
            vm = list(vm_)

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
        super().__init__()
        assert v1 != v2
        if v2 < v1:
            v2, v1 = v1, v2
        self.v1 = v1
        self.v2 = v2
    @property
    def vars(self):
        return (self.v1, self.v2)

    def rename_vars(self, remap):
        return unify(remap(self.v1), remap(self.v2))

    def _tuple_rep(self):
        return self.__class__.__name__, self.v1, self.v2

def unify(a, b):
    if a == b:
        return Terminal(1)
    if isinstance(a, ConstantVariable) and isinstance(b, ConstantVariable):
        if a.getValue(None) == b.getValue(None):  # should have a better way of doing this???
            # this should be handled in the above case???
            assert False
            return Terminal(1)
        else:
            return Terminal(0)
    return Unify(a,b)

@simplify.define(Unify)
def simplify_unify(self, frame):
    if self.v1.isBound(frame):
        self.v2.setValue(frame, self.v1.getValue(frame))
        return terminal(1)
    elif self.v2.isBound(frame):
        self.v1.setValue(frame, self.v2.getValue(frame))
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

    def __init__(self, result: Variable, head_vars: Tuple[Variable], body_res: Variable, aggregator :AggregatorOpBase, body :RBaseType):
        super().__init__()
        self.result = result
        self.body_res = body_res
        self.body = body
        self.head_vars = head_vars
        self.aggregator = aggregator

        assert result != body_res  # we require that there is some difference in variable so that there is someway that this can propagate through (maybe this should just rewrite the body itself before it attaches it?)

    @property
    def vars(self):
        return (self.result, self.body_res, *self.head_vars)
    @property
    def children(self):
        return (self.body,)

    def rename_vars(self, remap):
        return Aggregator(
            remap(self.result),
            tuple(remap(v) for v in self.head_vars),
            remap(self.body_res),
            self.aggregator,
            self.body.rename_vars(remap)
        )

    def rewrite(self, rewriter):
        return Aggregator(
            self.result,
            self.head_vars,
            self.body_res,
            self.aggregator,
            rewriter(self.body)
        )

    def _tuple_rep(self):
        return self.__class__.__name__, self.result, self.head_vars, self.body_res, self.body._tuple_rep()


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

            if not R.isEmpty():  # ignore the empty states
                v = self.body_res.getValue(frame)
                assert v is not InvalidValue  # if this happens some invalid R-expr was generated?
                for _ in range(R.multiplicity):
                    if agg_result is not None:
                        agg_result = self.aggregator.combine(agg_result, v)
                    else:
                        agg_result = v

        body = saturate(body, frame)

        loop(body, frame, loop_cb)

        if agg_result is None:
            return terminal(0)  # nothing from the aggregator
        else:
            self.result.setValue(frame, agg_result)
            return terminal(1)  # return that we are done and the result of aggregation has been computed

    # There also needs to be some handling in the case that the result variable
    # from the body is fully grounded, but the head variables are not grounded.
    # (Meaning that this is something like `f(X) += 5.`)

    return Aggregator(self.result, self.head_vars, self.body_res, self.aggregator, body)


@getPartitions.define(Aggregator)
def getPartitions_aggregator(self, frame):
    for p in getPartitions(self.body, frame):
        if p.variable in self.head_vars:
            # filter out the iterators that are going to yield unconsolidated results
            yield p


make_aggregator_loopable = Visitor()

@make_aggregator_loopable.define(Aggregator)
def make_aggregator_loopable_agg(R, frame):
    hs = set((*R.head_vars, R.body_res))
    if not isinstance(R.body, Partition) or set(R.body._unioned_vars) != hs:
        # then we want to make the body a partition so that we can loop the different expressions
        hs = tuple(hs)  # the order probably will impact the trie
        nb = partition(hs, [R.body.rewrite(lambda x: make_aggregator_loopable(x, frame))])
        nb = simplify(nb, frame, flatten_keys=True)
        # it might be important that we maintain the frame? or that we unify
        # these variables with their constant values

        return Aggregator(R.result, R.head_vars, R.body_res, R.aggregator, nb)
    return R.rewrite(lambda x: make_aggregator_loopable(x, frame))

@make_aggregator_loopable.define(Partition)
def make_aggregator_loopable_partition(R, frame=None):
    R = R.rewrite(lambda x: make_aggregator_loopable(x, frame))
    if isinstance(R, Partition):  # if the partition got deleted somehow then we don't want to fail on the next line
        R = simplify(R, frame, flatten_keys=True)
    return R




class ModedOp(RBaseType):
    def __init__(self, name, det, nondet, vars):
        super().__init__()
        self.det = det
        self.nondet = nondet
        self.name = name
        self.vars_ = vars
    @property
    def vars(self):
        return self.vars_
    def rename_vars(self, remap):
        return ModedOp(self.name, self.det, self.nondet, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.det is other.det and self.nondet is other.nondet
    def _tuple_rep(self):
        return (self.__class__.__name__, self.name, self.vars)
    def __eq__(self, other):
        return super().__eq__(other) and self.det is other.det and self.nondet is other.nondet
    def __hash__(self):
        return super().__hash__() ^ object.__hash__(self.det) ^ object.__hash__(self.nondet)

class IteratorFromIterable(Iterator):
    def __init__(self, variable, iterable):
        self.variable = variable
        self.iterable = iterable
    def bind_iterator(self, frame, variable, value):
        assert variable == self.variable
        if value in self.iterable:
            pass
        else:
            pass
    def run(self, frame):
        for v in self.iterable:
            yield {self.variable: v}
    @property
    def variables(self):
        return (self.variable,)


@simplify.define(ModedOp)
def modedop_simplify(self, frame):
    mode = tuple(v.isBound(frame) for v in self.vars)
    if mode in self.det:
        vals = tuple(v.getValue(frame) for v in self.vars)
        r = self.det[mode](*vals)
        if isinstance(r, FinalState):
            assert not isinstance(r, Terminal) or r.multiplicity <= 1  # force to be semi-det
            return r
        if r == ():
            return self  # made no progress
        for var, val in zip(self.vars, r):
            var.setValue(frame, val)
        return terminal(1)
    return self

@getPartitions.define(ModedOp)
def modedop_getPartitions(self, frame):
    mode = tuple(v.isBound(frame) for v in self.vars)
    if mode in self.nondet:
        # then this needs to get the iterator from the object and yield that
        # as a partition that can handle binding the particular variable

        vals = tuple(v.getValue(frame) for v in self.vars)
        r = self.nondet[mode](*vals)

        # these are cases which failed unification or something?  We need to
        # handle reporting errors in these cases as empty intersections
        assert r != () and not isinstance(r, FinalState)

        # TODO: this needs to handle all of the grounded variables first which
        # would have cases where we are checking if the value of a variable
        # unifies correctly

        for var, val in zip(self.vars, r):
            if hasattr(val, '__iter__'):
                # then this is a variable that we can iterate, so we want to do
                # that.  This should yield some iterator wrapper that is going
                # return the map to a variable.  This might also want to be able
                # to check contains, in which case, this should support the
                # overlapping behavior required for aggregation?

                yield IteratorFromIterable(var, val)
            elif not var.isBound(frame):
                yield SingleIterator(var, val)
