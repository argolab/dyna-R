import itertools
from collections import defaultdict
from typing import *

from .interpreter import *
from .terms import inline_all_calls
from .guards import Assumption, AssumptionListener, get_all_assumptions

class MemoContainer:

    body : RBaseType
    memos : Partition  # which is also an RBaseType

    def __init__(self, supported_mode: Tuple[bool], variables: Tuple[Variable], body: RBaseType, is_null_memo=False):
        # parameterization so of the memo table that _should not change_
        self.supported_mode = supported_mode
        self.variables = variables
        self.body = body
        self.is_null_memo = is_null_memo
        # if this is null, then when an update comes in, we have to recompute rather than being able to just delete
        # this is a property of the table, rather than where we are choosing to use it
        # TODO: the UnkMemo and the NullMemo should probably just be merged and then this should be the trigger between the two
        #  (Though this should maybe be more fine grained in that we might want null memos on some keys, but unk on others?)



        # the container of the memos themselves, this R-expr is modified (in
        # place) to add new memos if anyone else gets a direct reference to this
        # R-expr, then that could potentially cause issues (due to the not being
        # immutable)
        self.memos = Partition(variables, PrefixTrie(len(self.variables)))


        # for tracking anything that depends on this expression.
        self.assumption = None

        # this is the pointer that is actually used for computation by removing
        # the indirections through the "dyna base" and directly reference the
        # memo other memo tables.  In the case that something is rewritten such
        # that this is invalid, then the assumption will be invalidated and we
        # will be triggered.
        self._full_body = None

        # for identifying things we are currently computing in the case of a
        # cycle that can only be forward chained.
        self._error_cycle = set()

        self._setup_assumptions()


    def lookup(self, values, *, compute_if_not_set=True):
        assert len(values) == len(self.variables)
        r = partition_lookup(self.memos, values)

        # TODO: remove the flag
        assert compute_if_not_set != self.is_null_memo

        if r is not None or not compute_if_not_set:
            return r
        # then we are going to compute the value for this and then return the result

        # TODO: this should use the same storage as the object, with this as an external object this is annoying...
        assert values not in self._error_cycle
        self._error_cycle.add(values)

        nR = self.compute(values)

        self._error_cycle.remove(values)

        # this is not as efficient as it could be given that this could handle
        # adding something to the partition in the case that the keys overlap,
        # but otherwise this should be fine

        # nv = self.memos._children.copy()  # make a copy of this?  Though maybe we should just assume that we own it and update
        # nv.setdefault(values, []).append(nR)

        # nM = Partition(self.memos._unioned_vars, nv)
        # self.memos = nM

        # modify the data structure in place, so we are assuming that we own
        # this (which better be the case), though it breaks the "ideal" that
        # these structures are immutable....
        self.memos._children.setdefault(values, []).append(nR)

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
        nR = saturate(self._full_body, frame)
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

    def _setup_assumptions(self):
        self.assumption = Assumption()
        self.assumption_listener = AssumptionListener(self)
        self._full_body = inline_all_calls(self.body, set())

        all_assumptions = list(get_all_assumptions(self._full_body))

        for a in all_assumptions:
            a.track(self.assumption_listener)


    def invalidate(self):
        # in the case of a unk memo, this can just delete everything, but if it
        # is a null memo, then we are going to have to recompute or notify
        # anything that is dependant on us.  So this system needs to know what
        # behavior it is working under?


        # this needs to delete all of the memos
        # if this is null, then we are going to have to perform a full recompute



        # reset the assumption so that we are properly tracking
        self._setup_assumptions()

        #assert False

    def signal(self, msg):
        # in this case, something used for the computation was invalidated, so
        # we are are going to have to mark the entries that are in the table

        assert False

        pass



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
    key = tuple(v.getValue(frame) if v.isBound(frame) else None for v in self.variables)
    res = self.memos.lookup(key)

    # rename the variables and make new spaces for things that were not
    # referenced
    vmap = dict(zip(self.memos.variables, self.variables))
    res2 = res.rename_vars_unique(vmap.get)


    # run the new result once, which can
    return simplify(res2, frame)



# this is very similar to the unk memos, and partition, so going to write this
# seperate first, and then work on mergining them later
class NullMemo(RBaseType):

    def __init__(self, variables :Tuple[Variable], memos :MemoContainer):
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

    # we need to remap the variables so that we only pass the names that are the same on both sides
    f = Frame()
    for va, vb in zip(self.variables, self.memos.variables):
        if va.isBound(frame):
            vb.setValue(f, va.getValue(frame))
    vmap = dict(zip(self.memos.variables, self.variables))
    for it in getPartitions(self.memos.memos, f):
        yield RemapVarIterator(vmap, it, vmap[it.variable])

def _flatten_keys(save, R, frame):
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


def naive_converge_memos(*tables):
    # this just keeps recomputing all of the memo tables until there are no
    # changes.  In this case, we can

    done = False
    while not done:
        done = True
        # keep looping until the table is at a fixed point.

        for t in tables:
            # this maybe should be done once at the start instead of every time that we go around this loop?
            R = inline_all_calls(t.body, set())

            nR = simplify(R, Frame(), map_function=_flatten_keys)

            if t.memos != nR:
                # then we need to update the expression in the table with this
                t.memos = nR
                done = False


# we can have a pointer in an Rexpr where this should get replaced into it, in
# which case we can just take the new expression and identify if there is
# something?  There is going to have to be special handling for cases where
# there are aggregators, so those are going to have to be deleted, or rewritten
# such that the results of variables are set
class RArgument(RBaseType):
    pass

@simplify.define(RArgument)
def simplify_rargument(self, frame):
    r = frame.Rargument
    frame.Rargument = None
    return simplify(r, frame)


def construct_memotable(R):
    # construct a memo table, we are going to rewrite the R-expr such that we can determien all of the

    pass

class AgendaMessage(NamedTuple):
    table : MemoContainer  # the container that we are updating, this will be tracked via pointer instead of name
    key : Tuple[object]  # the key in the table, this should match the order of arguments as used by the table, None indicates variable not set

    # used in the case that this is an update, and we are able to just add these changes
    addition : RBaseType = None  # an Rexpr that we are adding to the table or None to indicate nothing here
    deletition : RBaseType = None# an Rexpr that we are removing form the table or None

    # if this is going through an unmemoized aggregator, then we might not be able to just directly modify the value in the table
    # so we are going to have to invalidate something and the perform a recomputation
    invalidation : bool = True  # just always going to be true for the moment...


def process_agneda_message(msg: AgendaMessage):
    # identify which rows are impacted, and the perform a computation on them

    # this needs to change the value in the memo table, which means that we are
    # also going to have to send notifications to anything that depends on this.


    # first we update the memo table with this new entry
    assert msg.deletition is None  # TODO handle this case

    # TODO: handle these cases
    assert msg.addition is None and msg.deletition is None


    t = msg.table

    if t.is_null_memo:
        pass
    else:
        # then we are just going to delete the memos as they are unk
        # we are also going to send messages to downstream entries
        t.filter(msg.key).delete_all()

    # send notifications to everything downstream

    # this is going to singnal that this key is invalidated, which will have to then be pushed forward
    t.assumption.signal(msg)






    # # for now just treat all of the messages as notifications that we need to recompute

    # R = inline_all_calls(msg.table.body, set())

    # # we are going to only compute this for the values that were notified of the changes
    # frame = Frame()
    # for val, var in zip(msg.key, msg.table.variables):
    #     if val:
    #         var.setValue(frame, val)

    # # this should identify which keys are changed, so we are going to have to identify which keys are changed
    # # if something has changed, then we are going to have to determine
    # nR = simplify(R, frame, map_function=_flatten_keys)







    # if msg.addition is None:
    #     assert msg.invalidation

    #     # going to recompute these slots on the frame, which will have something that is new.

    #     #for



    #     assert False  # TODO:
    # else:
    #     # modify the table with the new change (this should have a better interface)
    #     msg.table.memos._children.setdefault(msg.key, []).append(msg.addition)

    #     # now we are going to have to handle the assumptions that there is something new
    #     # which in this case is the addition
