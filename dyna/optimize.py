from collections import defaultdict
from typing import *

from .interpreter import *

def get_intersecting_constraints(R):
    yield R
    if not isinstance(R, Partition):
        for z in R.children:
            yield from get_intersecting_constraints(z)


def map_constraints_to_vars(constraints):
    ret = defaultdict(list)
    for f in constraints:
        for v in f.vars:
            ret[v].append(f)
    return ret


class RStrctureInfo:

    conjunctive_constraints : Dict[Variable,List[RBaseType]]

    all_constraints : Dict[Variable,List[RBaseType]]  # all constraints that match against a given expression

    alias_vars : Dict[Variable,Set[Variable]]

    exposed_variables : Set[Variable]

    frame : Frame

    def __init__(self, conjunctive_constraints=None, alias_vars=None, exposed_variables=None, frame=None):
        self.conjunctive_constraints = conjunctive_constraints or defaultdict(list)
        self.alias_vars = alias_vars or defaultdict(set)
        self.exposed_variables = exposed_variables
        self.frame = frame


    def recurse(self, *, frame=None):
        return RStrctureInfo(
            conjunctive_constraints=defaultdict(list, ((k, v.copy()) for k,v in self.conjunctive_constraints.items())),
            alias_vars=defaultdict(set, ((k, v.copy()) for k,v in self.alias_vars.items())),
            exposed_variables=self.exposed_variables,
            frame=frame or self.frame)

    def get_constraints(self, var, typ):
        for c in conjunctive_constraints[var]:
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
            # then we are going to rename these variables, and then add in unify constraints which could be later delated if considered save
            #assert all(n not in renames for n in info.exposed_variables)

            R = R.rename_vars(lambda x: renames.get(x,x))
            R = intersect(*(Unify(k, v) for k,v in renames.items()), R)
            return R

    return R

class OptimizeVisitor(Visitor):
    def __call__(self, R, info, *args, **kwargs):
        rr = super().__call__(R, info, *args, **kwargs)
        info.replace(R, rr)
        return rr

optimizer = OptimizeVisitor()

@optimizer.define(Partition)
def optimzier_partition(R, info):
    # we are going to want handle the fact that there are different branches of
    # disjunctive constraints. which means that we are giong to be entering a
    # lot of different contexts.

    def opt_mapper(save, R, frame):
        # we are going to want to call optimzie on all of these expressions
        # which is able ot identify cases where

        # we are going to identify which conjunctive constraints are used here first

        i2 = info.recurse(frame=frame)

        # add in the conjunctive constraints to this expression
        for var, cons in map_constraints_to_vars(get_intersecting_constraints(R)).items():
            i2.conjunctive_constraints[var] += cons

        rr = optimizer(R, i2)
        # this should construct aliased variables so that we can handle cases where there are expressions which

        rr = optimizer_aliased_vars(R, info)
        save(rr, frame)

    # ???: do we want to run the simplify before we get the callback?  That
    # could delete some constraint that we might be able to use for pattern
    # matching.  But I suppose that this tracks that there are
    res = simplify(R, info.frame, map_function=opt_mapper)


    return res


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


def run_optimizer(R, exposed_variables):
    # This is the entry point for the optimizer, we can use simplify in this
    # case as we it can run with the ground values of variables.  The
    # exposed_variables are the names of variables that we _have_ to keep
    # consistent as they are used by something external.  But any other
    # variables that are in this expression can be renamed or eleminated

    done = False
    exposed_constants = []
    frame = Frame()
    frame.memo_reads = False  # prevent memo tables from being read at this step so optimizations are not dependant
    while not done:
        last_R = R

        # print(R)
        # print(frame)
        # print('-'*50)

        R = simplify(R, frame)


        info = RStrctureInfo(exposed_variables=exposed_variables,frame=frame)
        info.conjunctive_constraints = map_constraints_to_vars(get_intersecting_constraints(R))
        info.all_constraints = map_constraints_to_vars(R.all_children())

        R = optimizer(R, info)

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

    return R
