from collections import defaultdict
from typing import *
from contextlib import contextmanager

from .interpreter import *

# def get_intersecting_constraints(R):
#     yield R
#     if not isinstance(R, Partition):
#         for z in R.children:
#             yield from get_intersecting_constraints(z)

# def map_constraints_to_vars(constraints):
#     ret = defaultdict(list)
#     for f in constraints:
#         for v in f.vars:
#             ret[v].append(f)
#     return ret

# def optimize_aliased_vars(R, parent_map=lambda x:x):
#     # optimize something like `X=Y, Z=&foo(X, Y)` to `X=Y, Z=&foo(X, X)`

#     varR = map_constraints_to_vars(get_intersecting_constraints(R))

#     alias_vars = defaultdict(set)

#     for var, Rs in varR:
#         for r in Rs:
#             if isinstance(r, Unify):
#                 assert r.a is not r.b
#                 alias_vars[r.a].add(r.b)
#                 alias_vars[r.b].add(r.a)

#     for var in alias_vars:
#         alias_vars[var].add(var)

#     done_progagate = False
#     while not done_progagate:
#         done_progagate = True
#         for var in alias_vars:
#             for v in alias_vars[var]:
#                 if alias_vars[var] != alias_vars[v]:
#                     r = alias_vars[v] | alias_vars[var]
#                     alias_vars[v] = r
#                     alias_vars[var] = r
#                     done_progagate = False

#     av2 = alias_vars.copy()

#     for var in alias_vars:
#         if all(isinstance(f, Unify) for f in varF[var]):
#             del av2[var]  # delete things that are already just a single constraint with the unify object, we don't want to add these again..


class RStrctureInfo(NamedTuple):

    conjunctive_constraints : Dict[Variable,Set[RBaseType]] = defaultdict(set)

    alias_vars : Dict[Variable,Set[Variable]]  = defaultdict(set)

    exposed_variables : Set[Variable] = set()

    frame : Frame = None

    @contextmanager
    def recurse(self):
        # want to restore the state as this explores different children branches
        # this should also identify cases where we can rewrite to remove expressions
        try:
            yield
        finally:
            pass

    def get_constraints(self, var, typ):
        for c in conjunctive_constraints[var]:
            if isinstance(c, typ):
                yield c


optimizer = Visitor()

# @optimizer.default
# def optimzier_default(R, info):
#     return R.rewrite(lambda x: optimizer(x, info))

@optimizer.define(Partition)
def optimzier_partition(R, info):
    # we are going to want handle the fact that there are different branches of
    # disjunctive constraints. which means that we are giong to be entering a
    # lot of different contexts.

    frame = Frame()
    frame.memo_reads = False

    def opt_mapper(R, frame):
        # we are going to want to call optimzie on all of these expressions
        # which is able ot identify cases where

        pass

    res = simplify(R, frame, map_function=...)




    raise NotImplementedError()




@optimizer.define(Unify)
def optimizer_unify(R, info):
    raise NotImplementedError()


def run_optimizer(R, exposed_variables):
    # This is the entry point for the optimizer, we can use simplify in this
    # case as we it can run with the ground values of variables.  The
    # exposed_variables are the names of variables that we _have_ to keep
    # consistent as they are used by something external.  But any other
    # variables that are in this expression can be renamed or eleminated

    done = False
    exposed_constants = []
    while not done:
        frame = Frame()
        frame.memo_reads = False  # prevent memo tables from being read at this step so optimizations are not dependant
        R = saturate(R, frame)

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

        info = RStrctureInfo(exposed_variables=exposed_variables)
        R = optimizer(R, info)
