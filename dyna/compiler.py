from typing import *

from .interpreter import *
from .terms import BuildStructure, Evaluate, ReflectStructure


# we are going to want to know which mode will come back from a given
# expression.  This means that we are looking for different expressions for
# different possible call modes.  Also we are interested in

get_mode = Visitor()

@get_mode.default
def get_mode_default(R):
    raise NotImplementedError()

@get_mode.define(ModedOp)
def get_mode_modedOp(R):
    return R.vars, R.det.keys(), R.nondet.keys()

@get_mode.define(Unify)
def get_mode_unify(R):
    return R.vars, ((True,False), (False,True), (True,True)), ()

@get_mode.define(Aggregator)
def get_mode_aggregator(R):
    assert False  # ??? need to look at the body or something
    return (*R.head_vars, R.result), ((True,)*len(R.head_vars)+(False,)), ()







class CompiledRexpr:
    """
    A representation of a compiled operation, or something that we are interested
    """

    rexpr :RBaseType  # the Rexpr that we are wrapping
    exposed_variables : Tuple[Variable]  # variables that are exposed.  The set of argument + return variables.  Anything not in this set is assumed not directly accessed
    compiled_modes :Dict[Tuple[bool], 'CompiledModedRexpr']  # compiled sequences mapped off the mode of the incoming

    def __init__(self, rexpr :RBaseType, exposed_variables: Tuple[Variable]):
        self.rexpr = rexpr
        self.exposed_variables = exposed_variables
        self.compiled_modes = {}


class CompiledModedRexpr:

    def __init__(self):
        pass




class CompiledFrame:

    def __init__(self, variables):
        self._values = len(variables)
        # the map from variables to slots should be done before we are running or something
        self._vmap = dict((v, i) for i,v in enumerate(variables))


class CompiledCallTerm(RBaseType):
    """The compiler uses a different operator for calling terms, as we want to be a
    bit more "static" about what return types we might get back.  We can
    additionally be more static about which expressions we are giong to use

    """

    def __init__(self, dyna_system, term_ref, call_mode):
        assert False








def compile(R, argument_variables, incoming_mode):
    # take an R-expr, and wrap it such that we compile it.  For now this should
    # just be the moded operations and determining which results are going to be
    # ground.  We then want to determine which order we are going to processes
    # constraints.  Finally there is going to have to be something about turning
    # this into a procedural order.  For something like loops, we are going to
    # have to determine which variables are getting grounded (meaning looking at
    # the iterators, and having a consistent way to get the same iterator)

    # ???: should this take the dyna system reference as this is having to
    # compile an expression in the context of other things which it might have
    # to infer.  If this is a recursive cycle, then are we going to identify
    # cases where there is something that it needs to guess the mode that will
    # be supported (or something?)

    pass
