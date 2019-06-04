import itertools
from collections import defaultdict


from .interpreter import *
from .terms import inline_all_calls


class MemoContainer:

    def __init__(self, supported_mode: Tuple[bool], variables: Tuple[Variable], body: RBaseType):
        assert len(supported_mode) == len(variables)
        self.supported_mode = supported_mode
        self.variables = variables
        self.body = body

        self.memos = {}

    def lookup(self, values, *, compute_if_not_set=True):
        assert len(values) == len(self.variables)
        key = []
        for var, imode, val in zip(self.variables, self.supported_mode, values):
            if imode:
                # if it is unbound, then we are going to have to iterate over
                # the domain of some variable, which means that we are going to
                # have to construct the domain of the variable (if we haven't
                # already) and then return a partition which iterates that
                # variable.  We might also want to delay, as constructing the
                # domain of a variable could be an expensive operation that we
                # want to avoid
                assert val is not None and val is not InvalidValue
                key.append(val)
        key = tuple(key)
        if key in self.memos:
            r =  self.memos[key]
            assert r is not None
            return r

        if not compute_if_not_set:
            # in this case, we are not computing the new value
            return None

        # there might be something else that should go here?
        self.memos[key] = None

        nR = self.compute(values)

        # for now we don't want to deal with cycles where the key might have been set by something else in the processes
        # so we are just going to /assert that away/
        assert self.memos[key] is None
        self.memos[key] = nR
        return nR

    def compute(self, values):
        # then we are going to determine what the result of this memoized value
        # is this requires constructing a new sub interpreter and using that to
        # set the values etc
        frame = Frame()
        for var, imode, val in zip(self.variables, self.supported_mode, values):
            if imode:
                var.setValue(frame, val)

        # this should put a marker down that this is computing for this space,
        # so that if it hits this space again, then I suppose that we are giong
        # to be forced to forward chain as then the value is not backwards
        # computable

        # determine the new body and frame
        nR = saturate(self.body, frame)
        nR = [nR]
        for var, imode in zip(self.variables, self.supported_mode):
            if not imode and var.isBound(frame):
                # then we need /somewhere/ to store the value of this /ground/ variable
                # so we are just going to add in unification with a constant for now
                # we might also want to instead use the partition system?
                nR.insert(0, Unify(var, constant(var.getValue(frame))))
            var._unset(frame)  # delete this from the frame for the next step

        nR = intersect(*nR)

        # we need to rewrite body such that it doesn't need this frame anymore
        # which means that we are going to remap all of the variables to their constant value
        d = dict((VariableId(k), constant(v)) for k,v in frame.items())
        if d:
            nR = nR.rename_vars(lambda x: d.get(x,x))

        return nR

    def __eq__(self, other):
        return self is other or \
            (type(self) is type(other) and
             self.body == other.body and
             self.variables == other.variables and
             self.supported_mode == other.supported_mode and
             self.memos == other.memos)


class MemoIterator(Iterator):
    # we might have multiple variables that are memoized, which means that we are going tohave to remap variables
    # in the case that a variable is not represented
    def __init__(self, variable, pos, memos):
        self.variable = variable
        self.pos = pos
        self.memos = memos
    def bind_iterator(self, frame, variable, value):
        for m in self.memos.memos.keys():
            # this should have some sort of index over these values
            # but instead we are giong to have that
            v = m[self.pos]
            if v == value:
                return True
        return False
    def run(self, frame):
        # we have to track which values we already emitted, as the curren data
        # structure is not a trie/allow for us to do this more efficiently
        emitted = set()
        for m in self.memos.memos.keys():
            v = m[self.pos]
            if v not in emitted:
                emitted.add(v)
                yield {self.variable: v}


class UnkMemo(RBaseType):

    def __init__(self, variables :Tuple[Variable], memos: MemoContainer):
        assert len(variables) == len(memos.variables)
        self.variables = variables
        self.memos = memos

    @property
    def vars(self):
        return self.variables

    def rename_vars(self, remap):
        return UnkMemo(tuple(remap(v) for v in self.variables), self.memos)

    def __eq__(self, other):
        return super().__eq__(other) and self.memos == other.memos

    def __hash__(self):
        return super().__hash__()

@simplify.define(UnkMemo)
def simplify_unkmemo(self, frame):
    # the idea should be that if we are handling different modes

    mode = tuple(v.isBound(frame) for v in self.variables)
    can_run = True
    for a, b in zip(mode, self.memos.supported_mode):
        if b and not a:
            can_run = False
    if not can_run:
        # then there isn't enough bound that we can attempt to look a memoized
        # value up
        return self
    key = tuple(v.getValue(frame) for v in self.variables)
    res = self.memos.lookup(key)

    # rename the variables and make new spaces for things that were not
    # referenced
    vmap = dict(zip(self.memos.variables, self.variables))
    res2 = res.rename_vars_unique(vmap.get)


    # run the new result once, which can
    return simplify(res2, frame)

class MemoContainer2:

    body : RBaseType
    memos : Partition  # which is also an RBaseType

    def __init__(self, supported_mode: Tuple[bool], variables: Tuple[Variable], body: RBaseType):
        self.supported_mode = supported_mode
        self.variables = variables
        self.body = body

        self.memos = Partition(variables, ())
        self._error_cycle = set()

    def lookup(self, values, *, compute_if_not_set=True):
        assert len(values) == len(self.variables)
        r = partition_lookup(self.memos, values)
        if r is not None or not compute_if_not_set:
            return r
        # then we are going to compute the value for this and then return the result

        # TODO: this should use the same storage as the object, with this as an external object this is annoying...
        assert values not in self._error_cycle
        self._error_cycle.add(values)

        nR = self.compute(values)

        self._error_cycle.remove(values)


        assert False  # this needs to save the result, but this needs to handle
        # that there might be different modes in which this works if the modes
        # do not match up, then the issue is that we might find something that
        # matches but that does not properly represent what we are computing.


    def compute(self, values):
        # then we are going to determine what the result of this memoized value
        # is this requires constructing a new sub interpreter and using that to
        # set the values etc
        frame = Frame()
        for var, imode, val in zip(self.variables, self.supported_mode, values):
            if imode:
                var.setValue(frame, val)

        # this should put a marker down that this is computing for this space,
        # so that if it hits this space again, then I suppose that we are giong
        # to be forced to forward chain as then the value is not backwards
        # computable

        # determine the new body and frame
        nR = saturate(self.body, frame)
        nR = [nR]
        for var, imode in zip(self.variables, self.supported_mode):
            if not imode and var.isBound(frame):
                # then we need /somewhere/ to store the value of this /ground/ variable
                # so we are just going to add in unification with a constant for now
                # we might also want to instead use the partition system?
                nR.insert(0, Unify(var, constant(var.getValue(frame))))
            var._unset(frame)  # delete this from the frame for the next step

        nR = intersect(*nR)

        # we need to rewrite body such that it doesn't need this frame anymore
        # which means that we are going to remap all of the variables to their constant value
        d = dict((VariableId(k), constant(v)) for k,v in frame.items())
        if d:
            nR = nR.rename_vars(lambda x: d.get(x,x))

        return nR


# this is very similar to the unk memos, and partition, so going to write this
# seperate first, and then work on mergining them later
class NullMemo(RBaseType):

    def __init__(self, variables :Tuple[Variable], memos :MemoContainer2):
        assert len(variables) == len(memos.variables)
        self.variables = variables
        self.memos = memos

    @property
    def vars(self):
        return self.variables

    def rename_vars(self, remap):
        return NullMemo(tuple(remap(v) for v in self.variables), self.memos)

    def __eq__(self, other):
        return super().__eq__(other) and self.memos == other.memos

    def __hash__(self):
        return super().__hash__()


@simplify.define(NullMemo)
def simplify_nullmemo(self, frame):
    mode = tuple(v.isBound(frame) for v in self.variables)
    can_run = True
    for a, b in zip(mode, self.memos.supported_mode):
        if b and not a:
            can_run = False
    if not can_run:
        # then there isn't enough bound that we can attempt to look a memoized
        # value up
        return self
    key = tuple(v.getValue(frame) if v.isBound(frame) else None for v in self.variables)

    res = self.memos.lookup(key, compute_if_not_set=False)
    if res is None:
        # then it wasn't found in the memo table, so we are giong to mark this as null
        return terminal(0)

    vmap = dict(zip(self.memos.variables, self.variables))
    res2 = res.rename_vars_unique(vmap.get)

    return simplify(res2, frame)


@getPartitions.define(NullMemo)
def getpartition_nullmemo(self, frame):
    # a major difference between null and unk is that we can use the memo table
    # as an iterator over the domain of variables, as if it /wasn't/ null, then
    # it would be contained in the memo table.
    #
    # This is a basic version of null memos, so we are just going to assume that
    # all of the argument variables are iterable

    #import ipdb; ipdb.set_trace()
    # for imode, var, i in zip(self.memos.supported_mode, self.variables, itertools.count()):
    #     if imode:
    #         # then this is memoized, so we are going to construct an iterator based off this memo table
    #         yield MemoIterator(var, i, self.memos)


    # this needs to loop over the partitions variables and the nremap the variables to the names that we are using in this expression


    # we need to remap the variables so that we only pass the names that are the same on both sides
    f = Frame()
    for va, vb in zip(self.variables, self.memos.variables):
        if va.isBound(frame):
            vb.setValue(f, va.getValue(frame))
    vmap = dict(zip(self.memos.variables, self.variables))
    for it in getPartitions(self.memos.memos, f):
        yield RemapVarIterator(vmap, it, vmap[it.variable])


def converge_memos(*tables):
    done = False
    while not done:
        done = True
        # keep looping until the table is at a fixed point

        for t in tables:
            # this maybe should be done once at the start instead of every time that we go around this loop?
            R = inline_all_calls(t.body)

            def flatten_keys(save, R, frame):
                # this needs to loop until all of the keys are ground, if we are
                # unable to ground everything, then we are going to have delayed
                # R-expr, but that needs to still avoid overlapping?
                if isinstance(R, FinalState):
                    save(R, frame)
                else:
                    # then we are going to loop and try and ground out more
                    def cb(R, frame):
                        #import ipdb; ipdb.set_trace()
                        save(R, frame)
                    loop(R, frame, cb, till_terminal=True)

            nR = simplify(R, Frame(), map_function=flatten_keys)

            if t.memos != nR:
                # then we need to update the expression in the table with this
                t.memos = nR
                done = False

            # else:
            #     import ipdb; ipdb.set_trace()
            # if len(t.memos.children) > 35:
            #     import ipdb; ipdb.set_trace()
