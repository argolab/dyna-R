from typing import *

from .interpreter import *
from .terms import CallTerm, BuildStructure, Evaluate, ReflectStructure


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





# class CompiledRexpr:
#     """
#     A representation of a compiled operation, or something that we are interested
#     """

#     rexpr :RBaseType  # the Rexpr that we are wrapping
#     exposed_variables : Tuple[Variable]  # variables that are exposed.  The set of argument + return variables.  Anything not in this set is assumed not directly accessed
#     compiled_modes :Dict[Tuple[bool], 'CompiledModedRexpr']  # compiled sequences mapped off the mode of the incoming

#     def __init__(self, rexpr :RBaseType, exposed_variables: Tuple[Variable]):
#         self.rexpr = rexpr
#         self.exposed_variables = exposed_variables
#         self.compiled_modes = {}


class CompiledModedExpression:

    def __init__(self):
        pass




class CompiledFrame:

    def __init__(self, variables):
        self._values = len(variables)
        # the map from variables to slots should be done before we are running or something
        self._vmap = dict((v, i) for i,v in enumerate(variables))



class CompiledVariable(Variable):
    """This should know which slot it is contained in.  this means that there
    should be some frame descriptor like object that it could use to track that
    the version is correct.  the frame itself then just becoems an array

    """
    pass




class CompiledCallTerm(CallTerm):
    """The compiler uses a different operator for calling terms, as we want to be a
    bit more "static" about what return types we might get back.  We can
    additionally be more static about which expressions we are giong to use

    """

    def __init__(self, var_map, dyna_syste, term_ref, compiled_ref):
        super().__init__(var_map, dyna_system, term_ref)
        self.parent_calls_blocker = []  # we are not using tihs

        self.compiled_ref = compiled_ref

    def rename_vars(self, remap):
        return CompiledCallTerm(dict((k, remap(v)) for k,v in self.var_map.items()), self.dyna_system, self.term_ref, self.compiled_ref)



class CompiledModedCall(RBaseType):

    def __init__(self, dyna_system, term_ref, call_mode :Tuple[bool], arguments :Tuple[Variable]):
        self.dyna_system = dyna_system
        self.term_ref = term_ref
        self.call_mode = call_mode
        self.result_mode = None  # TODO: need to know what is bound after this call is performed, and we are going to need to still plan which expressions in the return bit are still to get handled.
        self.arguments = arguments

        #
        self.returned_rexpr = None  # TODO: need to know what could possibly come back in this case

    @property
    def vars(self):
        return self.arguments



def replace_calls(R):
    if isinstance(R, CallTerm):
        return CompiledCallTerm(R.var_map, R.dyna_system, R.term_ref,
                                R.dyna_system.create_compiled_expression(R.term_ref, R.var_map.values()))
    if isinstance(R, (Evaluate, Evaluate_reflect, ReflectStructure)):
        raise RuntimeError('unable to compile this expression, it uses reflection, try the optimizer first')  # TODO: should be some typed error that we can throw
    return R.rewrite(replace_calls)


class CompileManager:
    """
    Wrap a compile task so that everything has access to the right operators
    """

    def __init__(self):
        self.operations = []  # List[Tuple[RBaseType,EvalFunction (if any)]]



# def plan_modes(R, bound_variables):

#     evaluated_operations = []  # as an operation is evaluated (meaning that its mode matches the current set of arguments, we are going to put it here and remove it from the R-expr)

#     def mode_rewriter(R):
#         nonlocal evaluated_operations
#         if isinstance(R, ModedOp):
#             assert False  # ....
#         elif isinstance(R, Aggregator):
#             # if the head arguments are known, then this can determine the
#             # resulting variable, but we are going to have to handle
#             pass

#         assert False

#     last_R = None
#     while last_R != R:
#         last_R = R

#         R = mode_rewriter(R)




def run_compiler(dyna_system, ce, R, incoming_mode):
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

    R = replace_calls(R)  # all of the calls in the method will now be replace with referneces to other compiled object
    # I suppose that we would like to plan as much stuff as




    return None  # indicating that there was some failure or that we are unable
                 # to do this at this time.
