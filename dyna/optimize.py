from collections import defaultdict
from typing import *

import networkx as nx

from .interpreter import *

def get_intersecting_constraints(R):
    yield R
    # include the aggregator as a constraint?  Though not purely intersecting
    if not isinstance(R, (Partition)):  # partition has different branches and aggregators can internally use loop which means their state might become split between different operators
        for z in R.children:
            yield from get_intersecting_constraints(z)


def map_constraints_to_vars(constraints):
    ret = defaultdict(list)
    for f in constraints:
        for v in f.vars:
            ret[v].append(f)
    return ret


def construct_intersecting(R):
    # remove the partitions from the R-expr so that we still capture some of the
    # expressions?
    if isinstance(R, Partition):
        return Terminal(1)
    return R.rewrite(construct_intersecting)

def sort_intersections(R):
    if isinstance(R, Intersect):
        return intersect(*sorted(R._children))
    return R.rewrite(sort_intersections)


def make_graph(R):  # not used atm
    G = nx.Graph()
    for cons in R.all_children():
        if not isinstance(cons, Intersect):
            assert not isinstance(cons, Partition)  # not supported atm
            G.add_node(cons)
            for i, v in enumerate(cons.vars):
                if not isinstance(v, ConstantVariable):
                    G.add_node(v)
                    G.add_edge(cons, v, eid=i)
            assert cons.vars
    return G


# def is_known_semidet(R):
#     from .terms import BuildStructure

#     if isinstance(R, ModedOp):
#         return not R.nondet  # if there is no non-det opereators, then it is semi-det, and we can duplicate it

#     if isinstance(R, BuildStructure):
#         return True

#     # if it is an aggregator, then it is semi-det, but it might involve loops
#     # internally (expensive stuff), so we are going to avoid duplicating them
#     # for now.  But idk how this will work out with aggregators, given that we
#     # are removing partitions from the consideration of splitting atm

#     # if isinstance(R, Aggregator):
#     #     return True  # then this returns 0 or 1... but
#     return False

def is_expression_semidet(R):
    from .terms import BuildStructure, ReflectStructure

    if isinstance(R, Intersect):
        return all(is_expression_semidet(c) for c in R.children)
    elif isinstance(R, ModedOp):
        return not R.nondet
    elif isinstance(R, (BuildStructure, Aggregator, Unify, ReflectStructure)):
        return True
    elif isinstance(R, Terminal) and R.multiplicity <= 1:
        return True

    return False


def split_heuristic(R, info=None):
    from .terms import CallTerm, BuildStructure  # sigh

    # determine which expressions we want to split out (if any) for the purposes
    # of making a more specalized compiled version.
    #
    # we want to get as much type information into the recursive calls, so we
    # are giong to try and pass relevant build structures into the new methods.
    # We should also attempt to merge any operation that only depends on a
    # subset of the variables that are in the new expression.  (eg if something
    # is like `int(X)`) then if X is included, we should push it down but also
    # keep a copy for ourselves)

    constraints = list(R.all_children())
    assert all(not isinstance(r, Partition) for r in constraints)  # TODO:? or just ignore these

    vmap = map_constraints_to_vars(constraints)

    calls = [c for c in constraints if isinstance(c, CallTerm)]
    ecalls = {}

    for c in calls:
        ecall = set((c,))  # the external call we are interested in constructing?
        oecall = None
        vs = set(c.vars)  # start with the variables that we are interested in
        while ecall != oecall:
            oecall = ecall.copy()
            def consider(c2):
                nonlocal ecalls, vs, vmap
                lv = set(v for v in c2.vars if len(vmap[v]) != 1 and not isinstance(v, ConstantVariable))
                if c2 in ecalls:
                    pass
                elif isinstance(c2, BuildStructure) and c2.result in vs:
                    ecall.add(c2)
                    vs |= set(c2.arguments)
                elif isinstance(c2, CallTerm):
                    # if this is something that is interesting, then we should
                    # include it, which means that there aren't additional variables
                    # that would have to be included

                    # TODO: this could potentially be representing that there is some merged expression, which could
                    # potentially return a higher multiplicity, as for those expressions this is not restricted.
                    # this should really ensure that the result would be from an aggregator, so that the multiplicity is at most 1

                    assert c2.dyna_system is c.dyna_system  # is this going to be a requirement, or just something that we should instead check, how to mix different "dynabases?"
                    if lv.issubset(vs) and lv:  # if there are some vars that intersect and it is a subset of the vars that we are interested in
                        ecall.add(c2)
                        assert isinstance(c2.term_ref, tuple)
                elif isinstance(c2, ModedOp):
                    # take moded ops if they are dealing with this operator
                    # specifically, so, if there was something like `f(X, X+1)`
                    # that would take the addition operator.  But this currently
                    # wouldn't take something like: `f(X,Z), Y=X+1, Z=Y+1`
                    # because it goes through two different operators to combine


                    # this can't change the resulting multiplicity for a given
                    # expression.  The builtins are all semi-determinstic for a
                    # given assignment.  Though they may provide a way to loop
                    # over the domain of a variable.  Though in that case, it
                    # would just have duplicated the builtin constraint which
                    # would not change the multiplicty overall.

                    if lv.issubset(vs) and lv:
                        ecall.add(lv)

            for c2 in constraints:
                consider(c2)

            if info is not None:
                for v in vs:
                    for c2 in info.conjunctive_constraints[v]:
                        consider(c2)

        if len(ecall) > 1:
            ecalls[c] = ecall

    # we are going to filter out calls that are not interesting.  maybe this should go into refine?
    ecalls = dict((k, v) for (k,v) in ecalls.items() if all((not v.issubset(y) or v is y) for y in ecalls.values()))

    return ecalls


def refine_splits(splits):
    # determine which operators we want to have in each split.  If some operator
    # is duplicated between two places, check that it semidet, or choose once
    # side or another for it, or just leave it in the origional program.

    if len(splits) == 1:
        return [list(splits.values())[0]]  # just return this single split

    raise NotImplementedError()  # if there are multiple places where we want to split

def remove_parent_call_trackers(R):
    from .terms import CallTerm
    if isinstance(R, CallTerm):
        # the parent_calls_blocker will be reset on this object, so that it can be inlined again
        return CallTerm(R.var_map, R.dyna_system, R.term_ref)
    return R.rewrite(remove_parent_call_trackers)

def make_split(R, splits):
    from .terms import CallTerm
    # the chunking operator.  Will break out an expression and determine which
    # variables are shared between the two sides.  From that point it will

    all_Rs = dict((b,b) for a in splits for b in a)

    def rewriter(r):
        if all_Rs.get(r) is r: # and not is_known_semidet(r):  # I suppose that keeping these semidet operations can only help?  we are going to allow earlier filtering of expressions, but we are going to have more variables shared..
            return Terminal(1)  # this is getting removed
        return r.rewrite(rewriter)
    R = rewriter(R)

    Rvs = set(R.all_vars())

    splits = [sort_intersections(remove_parent_call_trackers(Intersect(tuple(s)))).weak_equiv() for s in splits]

    additions = []
    for spr, spv in splits:
        ds = None
        for c in spr.all_children():
            if isinstance(c, CallTerm):
                ds = c.dyna_system
                break
        cv = dict((k,v) for k,v in spv.items() if v in Rvs)  # get the variables that are shared between the two expressions
        exposed_vars = set(cv.keys())

        mt = ds.create_merged_expression(spr, exposed_vars)
        additions.append(CallTerm(cv, ds, mt))

    return intersect(R, *additions)


class RStructureInfo:

    conjunctive_constraints : Dict[Variable,List[RBaseType]]  # all the current constraints in the partition + what is in the parent

    partition_constraints : Dict[Variable,List[RBaseType]]  # the constraints that are just in the current active partition

    all_constraints : Dict[Variable,List[RBaseType]]  # all constraints that match against a given expression

    alias_vars : Dict[Variable,Set[Variable]]

    exposed_variables : Set[Variable]

    frame : Frame

    def __init__(self, *, conjunctive_constraints=None, partition_constraints=None, all_constraints=None, alias_vars=None, exposed_variables=None, frame=None):
        self.conjunctive_constraints = conjunctive_constraints or defaultdict(list)
        self.partition_constraints = partition_constraints if partition_constraints is not None else self.conjunctive_constraints
        self.all_constraints = all_constraints
        self.alias_vars = alias_vars or defaultdict(set)
        self.exposed_variables = exposed_variables
        self.frame = frame


    def recurse(self, *, frame=None):
        return RStructureInfo(
            conjunctive_constraints=defaultdict(list, ((k, v.copy()) for k,v in self.conjunctive_constraints.items())),
            all_constraints=self.all_constraints,
            # don't pass alias vars, as we want to handle that at every scope seperatly
            exposed_variables=self.exposed_variables,
            frame=frame or self.frame)

    def get_constraints(self, var, typ):
        for c in self.conjunctive_constraints[var]:
            if isinstance(c, typ):
                yield c

    def replace(self, old_R, R):
        if old_R != R:
            # TODO: handle the old_R and delete it from the tracking

            # for when new expressions are generated, we want to track that which will allow for this
            for var, cons in map_constraints_to_vars(get_intersecting_constraints(R)).items():
                self.conjunctive_constraints[var] += cons
            for var, cons in map_constraints_to_vars(R.all_children()).items():
                self.all_constraints[var] += cons


def optimizer_aliased_vars(R, info):
    # this should take the variables and perform renaming on the variables.
    if info.alias_vars:
        # then we are going to want to determine if there are more variables at
        # the outer scope which are referencing these variables, which will
        # inform us if we can just delete these variables.  In this case, we
        # want to determine which expressions are

        alias_vars = info.alias_vars

        for k, v in alias_vars.items():
            v.add(k)  # add the var to its own collection

        exposed = set(info.exposed_variables)

        done_progagate = False
        while not done_progagate:
            done_progagate = True
            for var in alias_vars:
                for v in alias_vars[var]:
                    if alias_vars[var] != alias_vars[v]:
                        r = alias_vars[var] | alias_vars[v]
                        alias_vars[v] = r
                        alias_vars[var] = r
                        done_progagate = False

        # if there are any exposed variables in this expression, then we want to
        # use those instead of these variables we are going to have to add in
        # these variables.  Then we might be able to delete some of the
        # variables from the program

        for var in alias_vars:
            if alias_vars[var] & exposed:  # then there is something that is one of the exposed variables
                alias_vars[var] &= exposed

        renames = dict((a,b) for a,b in ((k, min(vs)) for k, vs in info.alias_vars.items()) if a != b)

        if renames:
            # then we are going to rename these variables, and then add in unify constraints which could be later deleted if considered safe
            #assert all(n not in renames for n in info.exposed_variables)

            R = R.rename_vars(lambda x: renames.get(x,x))
            ufs = [Unify(k, v) for k,v in renames.items() if k in exposed or v in exposed]
            if TRACK_CONSTRUCTED_FROM:
                for u in ufs: u._constructed_from = "optimizer alias vars"
            R = intersect(*ufs, R)
            return R

    return R

class OptimizeVisitor(RefinedVisitor):
    def __call__(self, R, info, *args, **kwargs):
        rr = super().__call__(R, info, *args, **kwargs)
        info.replace(R, rr)
        return rr

optimizer = OptimizeVisitor()

@optimizer.define(Partition)
def optimzier_partition(partition, info):
    from .terms import BuildStructure
    # we are going to want handle the fact that there are different branches of
    # disjunctive constraints. which means that we are giong to be entering a
    # lot of different contexts.

    check_values = [v for v in partition._unioned_vars if not v.isBound(info.frame)]

    def opt_mapper(save, R, frame):
        # we are going to want to call optimzie on all of these expressions
        # which is able ot identify cases where

        # we are going to identify which conjunctive constraints are used here first

        i2 = info.recurse(frame=frame)
        i2.partition_constraints = map_constraints_to_vars(get_intersecting_constraints(R))

        frame2 = Frame(frame)  # copy frame as we do not want to modify the state with these extra operators as they might not be properly unset later
        for var in check_values:
            if var.isBound(frame):
                # then we want to double check the value with all of the
                # constraints. that are outside of the partition, as these would
                # never work
                for c in info.conjunctive_constraints[var]:
                    if isinstance(c, (BuildStructure, ModedOp)):
                        cr = simplify(c, frame2)
                        if cr.isEmpty():
                            # then this branch does not do anything, so we are going to keep it around
                            return

        # add in the conjunctive constraints to this expression
        for var, cons in i2.partition_constraints.items():
            i2.conjunctive_constraints[var] += cons

        rr = optimizer(R, i2)
        # this should construct aliased variables so that we can handle cases where there are expressions which

        rr = optimizer_aliased_vars(R, i2)

        save(rr, frame)

    # ???: do we want to run the simplify before we get the callback?  That
    # could delete some constraint that we might be able to use for pattern
    # matching.  But I suppose that this tracks that there are
    res = simplify(partition, info.frame, map_function=opt_mapper)

    return res

@optimizer.define(Aggregator)
def optimize_aggregator(R, info):
    from .aggregators import AGGREGATORS, null_term
    from .builtins import binary_neq
    from .terms import BuildStructure

    # the body of an aggregator might loop over multiple values, so it can't exactly be considered as conjunctive with the outer frame
    # in that the outer frame can use constraints from the aggregator, but not vice versa
    i2 = info.recurse()
    i2.partition_constraints = map_constraints_to_vars(get_intersecting_constraints(R.body))


    body = optimizer(R.body, i2)

    # if there is only a single body and everything is semidet?  Though if this
    # is :=, then we need to handle that case specially.  That will include
    # checking if the result is null?  Though if there are not semi-det
    # operations
    #
    # I suppose that we could also check if there is some semiring between
    # different aggregators, though that would require checking that we have the builtins also defined?

    if isinstance(body, Intersect) and is_expression_semidet(body):
        # then all of the branches of the partition have been removed, so there
        # is only a single branch left.  We can try and determine if the
        # expression is semi-deterministic, so we can remove the operation
        is_colon_eq = R.aggregator is AGGREGATORS[':=']

        if not is_colon_eq:
            return intersect(unify(R.result, R.body_res), body)
        else:
            # handle :=
            if R.body_res.isBound(i2.frame):
                val = R.body_res.getValue(i2.frame)
                if val.arguments[1] == null_term:
                    return Terminal(0)
                else:
                    R.result.setValue(i2.frame, val.arguments[0])
                    return body
            else:
                return intersect(BuildStructure('$colon_line_tracking', R.body_res, (VariableId(), R.result)),
                                 binary_neq(R.result, constant(null_term), ret=constant(True)),
                                 body)

    # TODO: this should be able to consider if the resulting variable of
    # aggregation is already set in which case, this could eleminate branches of
    # the expression which would not match.  But in the case of something like
    # :=, this might override the value with something that does not match?
    # Also with something like max/min, then it would need to include all
    # branches which might produce a higher value

    return Aggregator(R.result, R.head_vars, R.body_res, R.aggregator, body)

@optimizer.define(Unify)
def optimizer_unify(R, info):
    # mark that these variables are unified together.  Also look if we can
    # delete this constraint, essentially deleting not needed variables from our
    # expression

    if (((len(info.all_constraints[R.v1]) == 1 and R.v1 not in info.exposed_variables) or
        (len(info.all_constraints[R.v2]) == 1 and R.v2 not in info.exposed_variables))
        and not (R.v1.isBound(info.frame) or R.v2.isBound(info.frame))):
        # just delete self, there is no use for this additional variable
        return Terminal(1)

    info.alias_vars[R.v1].add(R.v2)
    info.alias_vars[R.v2].add(R.v1)

    return R

def delete_useless_unions(R, info):
    # we need to identify branches of the union that we are unable to actually
    # use.  This means that we take a union and then identify that there are
    # branches that are not going to contribute
    #
    # only do this at the top level

    partitions = [p for p in get_intersecting_constraints(R) if isinstance(p, Partition)]

    deletes = {}

    #st = str(R)

    if partitions:
        for p in partitions:
            for kk, vv in p._children:
                for v in vv:
                    frame = Frame(info.frame)
                    for val, var in zip(kk, p._unioned_vars):
                        if val is not None:
                            var.setValue(frame, val)
                    i2 = info.recurse(frame=frame)
                    for var, cons in map_constraints_to_vars(get_intersecting_constraints(v)).items():
                        i2.conjunctive_constraints[var] += cons

                    # there could be more rounds of running the optimizer/simplification here, but this should be sufficient for our current use cases....
                    rr = simplify(intersect(v, R), i2.frame)
                    if not rr.isEmpty():
                        rr = optimizer(rr, i2)

                    if rr.isEmpty():
                        # then we have found something that we can delete, so mark that
                        deletes.setdefault(p, set()).add((kk, v))

    #assert st == str(R)

    if deletes:
        def rewriter(r):
            if isinstance(r, Partition):
                if r in deletes:
                    d = deletes[r]
                    np = PrefixTrie(len(r._unioned_vars))
                    for k, vv in r._children.items():
                        for v in vv:
                            if (k, v) not in d:
                                np.setdefault(k,[]).append(v)
                    return Partition(r._unioned_vars, np)
                else:
                    return r
            return r.rewrite(rewriter)
        R0 = R
        R1 = rewriter(R)
        R = simplify(R1, info.frame)

    return R


def infered_constraints(R, dyna_system, info):
    # infer constraints that can be added to the expression based of existing
    # conjunctions of constraints.
    #
    # this uses dyna_system.infered_constraints{,_index} to determine which
    # expressions we might be able to add to a program
    pass


def lift_up_equivalent_constraints(R):
    # if there are some conjunctive constraints that are in an expression which
    # is not conjunctive (Aggregators and partition), then we should attempt to
    # lift these expressions up such that we can
    pass


def run_optimizer_local(R, exposed_variables):
    """This is the entry point for the optimizer that is _only_ going to operate on
    a single R-expr.  This will return a new R-expr that is semantically
    equivalent and at least the variables listed in exposed_variables will have
    the _same_ name (this is not gaurenteed for any other variables which might
    be eleminated) """

    ex = set(R.all_vars()) & set(exposed_variables)

    assumptions = set()

    exposed_constants = []
    frame = Frame()
    frame.in_optimizer = True  # prevent memo tables from being read at this step so optimizations are not dependant
    frame.assumption_tracker = assumptions.add
    while True:
        last_R = R

        # print(R)
        # print(frame)
        # print('-'*50)

        R = saturate(R, frame)
        if R.isEmpty():
            break

        info = RStructureInfo(exposed_variables=exposed_variables,frame=frame)
        info.partition_constraints = info.conjunctive_constraints = map_constraints_to_vars(get_intersecting_constraints(R))
        info.all_constraints = map_constraints_to_vars(R.all_children())

        R0 = R
        R = optimizer(R, info)
        R1 = R
        R = saturate(R, info.frame)

        if R.isEmpty():
            break

        R = delete_useless_unions(R, info)

        R = optimizer_aliased_vars(R, info)

        if R == last_R:
            break

    if frame:
        # then there are constants that we can embed in the program rather than having to perform reads on the frame
        def rn(x):
            if x.isBound(frame):
                return constant(x.getValue(frame))
            return x
        R = R.rename_vars(rn)
        for var in exposed_variables:
            if var.isBound(frame):
                exposed_constants.append(Unify(constant(var.getValue(frame)), var))

    R = intersect(*exposed_constants, R)

    # Rz = sort_intersections(construct_intersecting(R))

    # # G = make_graph(Rz)

    # # import matplotlib.pyplot as plt
    # # labels = {}
    # # for v in Rz.all_vars():
    # #     labels[v] = str(v)

    # # nx.draw(G, with_labels=True)
    # # plt.show()
    # # assert nx.is_connected(G)

    # sp = split_heuristic(Rz)

    # import ipdb; ipdb.set_trace()

    assert ex.issubset(set(R.all_vars())) or R.isEmpty()

    return R, assumptions


def run_optimizer(R, exposed_variables):
    """this will potentially construct new method names that are the same for
    different operations.  These operations will be saved in the dyna_system."""

    rr, assumptions = run_optimizer_local(R, exposed_variables)

    splits = split_heuristic(construct_intersecting(rr))

    assert isinstance(rr, RBaseType)

    if not splits:
        return rr, assumptions

    rsplits = refine_splits(splits)

    # this now is a new program with some common states combined out into
    # external calls.
    mk = make_split(rr, rsplits)

    assert isinstance(mk, RBaseType)

    return mk, assumptions




# TODO:
#  1. if we assume that builtins can only return multiplicity 1, then we can eleminate duplicated constraints.
#     maybe we could just put a marking on builtins that might return a higher multiplicity, so that we can eleminate them.  Or if higher multiplicities are only allowed by binding a variable multiple times, then those should always be "exposed" at least at R-expr level, so we could elminate those builtins.
#     In that case, if a user wrote a builtin multiple times, there would be two different variables that would need to be attached to the multiplicity, which does not quite give the behavior that we want.  This instead would still need to track that the two builtins would require a square on the multiplicity
#     could have some R-expr which just takes the multiplicity to some higher power?  Then it could combine anything that it wanted?  Though that probably is not that useful in general
#     - this only lets it remove if _all_ of the variables are already the same.  Otherwise something like range could have an issue where two variables are merged inapproperatly?  Maybe the result variables could be merged in some cases, or if it knew that there was a functional relationship between two of the variables, then it would
