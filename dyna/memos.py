import itertools
from collections import defaultdict
from typing import *

from .interpreter import *
from .terms import inline_all_calls
from .guards import Assumption, AssumptionListener, get_all_assumptions
from .agenda import push_work
from .prefix_trie import zip_tries

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

        # for identifying things we are currently computing via backchaining in
        # the case of a cycle that can only be forward chained.
        self._error_cycle = set()

        self._setup_assumptions()

        if self.is_null_memo:
            # then we need to init the table as this is a null guess for all of
            # the entries, which means that we are likely inconsistent with the guess

            push_work(lambda: refresh_whole_table(self))


    def lookup(self, values):
        assert len(values) == len(self.variables)
        r = partition_lookup(self.memos, values)

        # # TODO: remove the flag
        # assert compute_if_not_set != self.is_null_memo

        if r is not None or self.is_null_memo:
            return r
        # then we are going to compute the value for this and then return the result

        # TODO: this should use the same storage as the object, with this as an external object this is annoying...
        assert values not in self._error_cycle
        self._error_cycle.add(values)

        nR = self.compute(values)

        self._error_cycle.remove(values)

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
        # this identifies which expression this memoized expression is dependent
        # on.  It inlines everything that it can into full_body and then
        # collects all assumptions.  These assumptions contain other memo tables
        # that we will read from as well as the definitions of different rules
        # (so that they can be changed later)

        self.assumption = Assumption('memo container')
        self.assumption_listener = AssumptionListener(self)
        self._full_body = inline_all_calls(self.body, set())

        all_assumptions = set(get_all_assumptions(self._full_body))

        for a in all_assumptions:
            a.track(self.assumption_listener)

    def invalidate(self):
        # In the case of an invalidation, then the assumption has changed in
        # such a way that we are unable to partially update ourselves.  So we
        # are going to have to delete everything and then recompute from scratch.


        # just delete all of the memos
        self.memos = Partition(self.variables, PrefixTrie(len(self.variables)))

        assumption = self.assumption

        # reset the assumption so that we are properly tracking
        self._setup_assumptions()

        # signal anything that depends on us
        assumption.invalidate()

        # if null, with a new empty table, we need to recompute all of the
        # initial values
        if self.is_null_memo:
            push_work(lambda: refresh_whole_table(self))


    def signal(self, msg):
        # an assumption can also send a more fine grained notification that
        # something has changed.  In this case the signal will key the key in
        # _another table_ that has changed, so we are going to identify _all_
        # keys in this table which might be impacted.  We will push
        # notifications to the keys in our table

        def has_ra(R):
            for c in R.all_children():
                if isinstance(c, ForwardMemoHole):
                    return True
            return False

        argument_variables = tuple(VariableId() for _ in range(len(msg.key)))

        propagators = []

        for rp, variables in rewrite_to_propagate(self._full_body, msg.table, ForwardMemoHole):
            r = True
            m = {}
            for var, val, av in zip(variables, msg.key, argument_variables):
                if isinstance(var, ConstantVariable):
                    if val is not None and var.getValue(None) != val:
                        r = False
                m[var] = av

            if not r:
                continue

            rp = rp.rename_vars(lambda x: m.get(x,x))

            for pt in split_partitions(rp):
                # if this doesn't have the arguments, then we are giong to want to simplify this or
                if has_ra(pt):
                    # then this is something that we are going to have to handle in that
                    propagators.append(pt)

        if not propagators:
            return

        # then we are going to merge the propagators together and determine
        # which keys are impacted.  we can then use the set of the keys to mark
        # those entries as having to be recomputed

        res = partition(self.variables, propagators)

        frame = Frame()
        for var, val in zip(argument_variables, msg.key):
            if val is not None:
                var.setValue(frame, val)

        nRes = simplify(res, frame, flatten_keys=True, reduce_to_single=False)

        if nRes.isEmpty():
            return

        refresh_keys = set(nRes._children.keys())

        # now we are going to push invalidations/notifications to ourselves to recompute these keys

        for k in refresh_keys:
            msg = AgendaMessage(table=self, key=k)
            push_work(lambda: process_agenda_message(msg))


class RMemo(RBaseType):
    """
    Represent the memo table inside of the R expression

    Null vs unk is controlled by the memo table itself instead of inside of the R-expr.
    """

    def __init__(self, variables :Tuple[Variable], memos: MemoContainer):
        super().__init__()
        assert len(variables) == len(memos.variables)
        self.variables = variables
        self.memos = memos

    @property
    def vars(self):
        return self.variables

    def rename_vars(self, remap):
        return RMemo(tuple(remap(v) for v in self.variables), self.memos)

    def __eq__(self, other):
        # if there are two different memo tables of the same thing, then maybe these should report as equal?
        return super().__eq__(other) and self.memos == other.memos

    def __hash__(self):
        return super().__hash__()


@simplify.define(RMemo)
def simplify_memo(self, frame):
    # the idea should be that if we are handling different modes

    if not frame.memo_reads:
        # then we are not allowed to perform any reads of a memo table
        return self

    frame.assumption_tracker(self.memos.assumption)

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

    if res is None:
        return terminal(0)

    # rename the variables and make new spaces for things that were not
    # referenced
    vmap = dict(zip(self.memos.variables, self.variables))
    res2 = res.rename_vars_unique(vmap.get)

    # run the new result once, which can
    return simplify(res2, frame)


@getPartitions.define(RMemo)
def getPartition_memos(self, frame):
    if not self.memos.is_null_memo or not frame.memo_reads:
        # then we can not use this table to iterate as we do not know all of the non-null values
        return

    f = Frame()
    for va, vb in zip(self.variables, self.memos.variables):
        if va.isBound(frame):
            vb.setValue(f, va.getValue(frame))
    vmap = dict(zip(self.memos.variables, self.variables))
    for it in getPartitions(self.memos.memos, f):
        yield RemapVarIterator(vmap, it, vmap[it.variable])


@get_all_assumptions.define(RMemo)
def get_assumptions_memos(self):
    yield self.memos.assumption


# def _flatten_keys(save, R, frame):
#     """
#     THIS IS THE WORSE, MOST BUGGY METHOD IN THE ENTIRE PROGRAM.  IT SUCK SOOOOOO BAD.  ITS SHORT LENGTH IS VERY DECEPTIVE
#     """

#     # this needs to loop until all of the keys are ground, if we are
#     # unable to ground everything, then we are going to have delayed
#     # R-expr, but that needs to still avoid overlapping?
#     # if 'ddd' in frame:
#     #     import ipdb; ipdb.set_trace()
#     if isinstance(R, FinalState):
#         save(R, frame)
#     else:
#         # then we are going to loop and try and ground out more
#         def cb(R, frame2):
#             # if frame != frame2:
#             #     import ipdb; ipdb.set_trace()
#             # this needs to save any additional variables that are set in the frame into the R-expr
#             # the loop method can construct new copies of the frame, and that might be over variables that are not going to be saved by the partition

#             # if frame != frame2:
#             #     # FML
#             #     R = R.rename_vars(lambda x: constant(x.getValue(frame2)) if (x.isBound(frame2) and not x.isBound(frame) and x not in save.unioned_vars) else x)

#             # double FML
#             r2 = R.rename_vars_unique(lambda x: constant(x.getValue(frame2)) if x.isBound(frame2) else (x if x in save.unioned_vars else None))

#             save(r2, frame2)
#         loop(R, frame, cb, best_effort=True)


def naive_converge_memos(*tables):
    # this just keeps recomputing all of the memo tables until there are no
    # changes.  This does not use an agenda and can be compared to naive
    # forwardchaining as in datalog

    done = False
    while not done:
        done = True
        # keep looping until the table is at a fixed point.

        for t in tables:
            # this maybe should be done once at the start instead of every time that we go around this loop?
            R = inline_all_calls(t.body, set())

            nR = simplify(R, Frame(), flatten_keys=True)

            if t.memos != nR:
                # then we need to update the expression in the table with this
                t.memos = nR
                done = False


def refresh_whole_table(table):
    # this is what gets the memoization processes started once we have made a
    # guess.  It performs a computation of the entire table using the program
    # and then will signal anything that might depend on the changes.
    #
    # Eg, in the case of fib, this is going to identify that fib(0) = 0 and
    # fib(1) = 1 are inconsistent with the guess that the whole table is null

    nR = simplify(table._full_body, Frame(), flatten_keys=True, reduce_to_single=False)

    if table.memos != nR:
        # then we are going to have to signal these entries, which means

        old_memos = table.memos
        table.memos = nR

        signals = []

        # determine which entries have changed
        for key, a, b in zip_tries(old_memos._children, nR._children):
            if a != b:
                # then this value has changed, and we are going to have to signal anything that depends on this

                msg = AgendaMessage(table=table, key=key)

                # these things are going to have to push to the agenda that they have been modified
                table.assumption.signal(msg)


class ForwardMemoHole(RBaseType):
    pass

@simplify.define(ForwardMemoHole)
def simplift_memohole(self, frame):
    return Terminal(1)

class AgendaMessage(NamedTuple):
    table : MemoContainer  # the container that we are updating, this will be tracked via pointer instead of name
    key : Tuple[object]  # the key in the table, this should match the order of arguments as used by the table, None indicates variable not set

    # used in the case that this is an update, and we are able to just add these changes
    addition : RBaseType = None  # an Rexpr that we are adding to the table or None to indicate nothing here
    deletion : RBaseType = None# an Rexpr that we are removing form the table or None

    # if this is going through an unmemoized aggregator, then we might not be able to just directly modify the value in the table
    # so we are going to have to invalidate something and the perform a recomputation
    invalidation : bool = True  # just always going to be true for the moment...


def process_agenda_message(msg: AgendaMessage):
    # the msg contains a pointer to the table and which key needs to be
    # updated/invalidated.
    #
    # This method does the computation of the impacted key (including handling
    # cases with unground variables, meaning something like `fib(2) = 1` is
    # really backed off to `fib(2) = ???` where the `???` is an unground
    # variable that needs to get filled in and refined.
    #
    # for everything that has been identified as changing, it signals any
    # downstream dependants and then those dependants are responsible for
    # enqueuing their own refresh updates to the agenda  as needed



    # first we update the memo table with this new entry
    assert msg.deletion is None  # TODO handle this case

    # TODO: handle these cases
    assert msg.addition is None and msg.deletion  is None


    t = msg.table

    if t.is_null_memo:
        frame = Frame()
        for var, val in zip(t.variables, msg.key):
            if val is not None:
                var.setValue(frame, val)
        nR = simplify(t._full_body, frame, flatten_keys=True, reduce_to_single=False)
        if nR.isEmpty():
            return

        # this needs to handle if the partition does the single

        tf = t.memos._children.filter(msg.key)
        tn = nR._children


        changes = []

        # we are going to identify which keys are changes and then update those
        for key, a, b in zip_tries(tf, tn):
            if a != b:
                changes.append((key, b))  # given that we are iterating the table, we don't want to make changes to the table while we are iterating.  So we are instead going

        for key, value in changes:
            # we are going to write this memo to the table
            # and then also notify anything that is downstream that might depend on this

            # this was a fully recompute, so we are going to replace everything for this key rather than just update
            t.memos._children[key] = value

            mm = AgendaMessage(table=t, key=key)  # make a new message, as this might be more fine grained than before
            t.assumption.signal(mm)

    else:
        # then we are just going to delete the memos as they are unk
        # we are also going to send messages to downstream entries
        t.memos._children.filter(msg.key).delete_all()
        t.memos._hashcache = None

        # send notifications to everything downstream

        # this is going to singnal that this key is invalidated, which will have to then be pushed forward
        t.assumption.signal(msg)


def rewrite_to_memoize(R, mem_variables=None, is_null_memo=False):
    if isinstance(R, Aggregator):
        # then we are going mark that we require the keys for now I suppose?
        # that should let us memoize anything that is fully determined, but if
        # is unk, then we are not going to be able to run in modes that don't
        # yet have all of the values.  So that still needs to be handled.

        variables = R.head_vars + (R.body_res,)
        assert mem_variables is None  # TODO: handle selecting which variables that we want to memoize

        memos = MemoContainer((True,)*len(R.head_vars)+(False,), variables, R.body, is_null_memo=is_null_memo)
        return Aggregator(R.result, R.head_vars, R.body_res, R.aggregator, RMemo(variables, memos))

    elif isinstance(R, Partition):
        variables = R.unioned_vars

        assert mem_variables is not None  # we need to know which variables are going to need to be present to perform queries (aka don't want to query on the result variables as we likely can't easily compute on them)

        mode = tuple(v in mem_variables for v in variables)
        memos = MemoContainer(mode, variables, R, is_null_memo=is_null_memo)
        return RMemo(variables, memos)
    else:
        raise NotImplementedError()  # TODO:??? maybe just walk through the structure, or just wrap this in a partition, but then need to figure out what variables are going to be shared


def split_partitions(R):
    # split the program into different branches where there are no partitions,
    # so that we can determine which branches are going to actually contribute.
    # If there are multiple partitions, then we are going to have to handle that

    # we are going to find some partition, if there is none, then we are just going to yield this result

    partition = None
    for c in R.all_children():
        if isinstance(c, Partition):
            partition = c
            break
    if partition is None:
        yield R
        return

    for k, vv in partition._children:
        for v in vv:
            def rewriter(R):
                if R is partition:
                    # then we are going to replace this partition with the branch
                    inter = []
                    for var, val in zip(partition._unioned_vars, k):
                        if val is not None:
                            inter.append(Unify(var, constant(val)))
                    inter.append(v)
                    return intersect(*inter)
                else:
                    return R.rewrite(rewriter)

            z = rewriter(R)
            assert R != z
            yield from split_partitions(z)


def rewrite_to_propagate(R, table, replace_with):
    # Identify places in R which referencing the memo table then replace those
    # with a new expression and return the names of the variables that attach at
    # that point.
    #
    # we additionally _delete_ from the program, as given that we only have a
    # single key that we are receiving a message from, we are not going to
    # properly compute the value of an aggregator unless we go and identify all
    # of the other keys that contribute.


    counter = 0
    max_counter = 1
    do_rewrite = 0
    variables = None
    def rewriter(R):
        nonlocal counter, do_rewrite, variables
        if isinstance(R, RMemo):
            if R.memos == table:  # then this is something that needs to be changed
                z = R
                if counter == do_rewrite:
                    variables = R.variables
                    z = replace_with()  # maybe this should get passed the R structure or something, so that we can use that to track the mapping?
                counter += 1
                return z
            return R  # this is another memo table that we are not rewriting atm
        elif isinstance(R, Aggregator):
            # then we want to remove the aggregator as we are unable to
            # determine which operations should be considered to be going
            # forward.  This means that the result of an aggregation will not be
            # determined, but rather we are going to just avoid performing that
            # computation (thus having an over estimate)
            return rewriter(R.body)
        else:
            return R.rewrite(rewriter)
    while True:
        z = R.rewrite(rewriter)
        if counter == 0:  # there is nothing that matched the memo table, so we are done
            return
        yield z, variables
        if max_counter < counter:
            max_counter = counter
        do_rewrite += 1
        if do_rewrite >= max_counter:
            break
        counter = 0
