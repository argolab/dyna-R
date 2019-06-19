from typing import *

from .interpreter import *
from .terms import CallTerm, BuildStructure, Evaluate, ReflectStructure, Evaluate_reflect


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


abstract_outmodes = Visitor()

@abstract_outmodes.default
def abstract_outmodes_default(R, bound):
    raise NotImplementedError()

@abstract_outmodes.define(ModedOp)
def abstract_outmodes(R, bound):
    mode = tuple(v.isBound(bound) for v in self.vars)
    if mode in self.det:
        # then we can evaluate this expression
        f = self.det[mode]
        def ev(frame):
            nonlocal f
            vals = tuple(v.getValue(frame) for v in self.vars)
            r = f(*vals)
            if isinstance(r, FinalState):
                # then we need to end this expression and allow this to continue running
                # in the case
                assert isinstance(r, Terminal)  # TODO: handle
                if r.multiplicity == 0:
                    return False  # then this failed
                elif r.multiplicity == 1:
                    return True
                else:
                    assert False  # ???

            try:
                for var, val in zip(self.vars, r):
                    var.setValue(frame, val)
            except UnificationFailure:
                return False

            return True  # indicate that this was successful
        return  [
            # what it rewrites as, what would become bound, some function that needs to be evaluated
            (Terminal(1), self.vars, ev),
            # there could be more expressions here, but these do not necessary
        ]
    if mode in self.nondet:
        # then we want to be able to iterate these variables, meaning that we should return that
        #
        assert False  # TODO

    return None  # indicates that there is nothing that we can do here


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

    def __init__(self, R, exposed_vars):
        self.operations = []  # List[Tuple[RBaseType,EvalFunction (if any)]]
        self.R = R
        self.bound_variables = dict((v, False) for v in set(R.all_vars()))
        for v in exposed_vars:
            self.bound_variables[v] = True

    def run(self):
        # we are going to keep performing rewrites to remove operations that we
        # are "running."  We will be tracking which expressions are

        def rewriter(R):
            pass




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

    R = replace_calls(R)  # all of the calls in the method will now be replace
                          # with referneces to other compiled object

    # normalize the expression such that we don't have to handle
    exposed_vars = ce.variable_order
    vm = dict((v,VariableId(i)) for i,v in enumerate(exposed_vars))
    R = R.rename_vars_unique(vm.get).weak_equiv(ignored=exposed_vars)







    return None  # indicating that there was some failure or that we are unable
                 # to do this at this time.
