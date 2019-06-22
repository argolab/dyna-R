from typing import *

from .interpreter import *
from .terms import CallTerm, BuildStructure, Evaluate, ReflectStructure, Evaluate_reflect
from .guards import remove_all_assumptions


# we are going to want to know which mode will come back from a given
# expression.  This means that we are looking for different expressions for
# different possible call modes.  Also we are interested in

# get_mode = Visitor()

# @get_mode.default
# def get_mode_default(R):
#     raise NotImplementedError()

# @get_mode.define(ModedOp)
# def get_mode_modedOp(R):
#     return R.vars, R.det.keys(), R.nondet.keys()

# @get_mode.define(Unify)
# def get_mode_unify(R):
#     return R.vars, ((True,False), (False,True), (True,True)), ()

# @get_mode.define(Aggregator)
# def get_mode_aggregator(R):
#     assert False  # ??? need to look at the body or something
#     return (*R.head_vars, R.result), ((True,)*len(R.head_vars)+(False,)), ()


abstract_outmodes = Visitor(track_source=False)

@abstract_outmodes.default
def abstract_outmodes_default(R, bound):
    raise NotImplementedError()

@abstract_outmodes.define(ModedOp)
def abstract_outmodes_modedop(self, manager: 'CompileManager'):
    bound = manager.bound_variables
    mode = tuple(bound[v] for v in self.vars)
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


@abstract_outmodes.define(Unify)
def abstract_outmodes_unify(self, manager):
    bound = manager.bound_variables
    if bound[self.v1] and bound[self.v2]:
        # then we are just going to check equlaity
        def check_equals(frame):
            return self.v1.getValue(frame) == self.v2.getValue(frame)

        return [
            (Terminal(1), (), check_equals)
        ]
    else:
        a, b = self.v1, self.v2
        if bound[b]:
            a,b = b,a
        if bound[a]:
            def copy_var(frame):
                b.setValue(frame, a.getValue(frame))
                return True  # this doesn't check anything as it isn't bound
            return [
                (Terminal(1), (b,), copy_var)
            ]




# class CompiledModedExpression:

#     def __init__(self):
#         pass




class CompiledFrame:

    def __init__(self, variables):
        self._values = [None]*len(variables)
        # the map from variables to slots should be done before we are running or something
        self._vmap = dict((v, i) for i,v in enumerate(variables))

    def __contains__(self, varname):
        import ipdb; ipdb.set_trace()
        raise RuntimeError('The compiled frame should not be using contained in to determine the binding state of a variable')

    def __getitem__(self, varname):
        # this should instead
        key = self._vmap[varname]
        return self._values[key]

    def get(self, varname, default):
        return self[varname]

    def __setitem__(self, varname, value):
        key = self._vmap[varname]
        self._values[key] = value
        return value

    def _frame_setvalue(self, varname, value):
        key = self._vmap[varname]
        self._values[key] = value


class CompiledVariable(Variable):
    """This should know which slot it is contained in.  this means that there
    should be some frame descriptor like object that it could use to track that
    the version is correct.  the frame itself then just becoems an array

    """
    def __init__(self):
        assert False  # TODO

    pass


class EnterCompiledCode(RBaseType):
    """This is the R-expr that is returned by the Context.lookup_term when it is
    referring to something that is compiled.  When this successfully matches
    against the mode, then it will call into the compiled code.  This should be
    only run inside of the interpreter where we are checking if we are in a mode
    that we have currently compiled code for

    """

    def __init__(self, handle, variables):
        super().__init__()
        self.handle = handle
        self.variables = variables

    @property
    def vars(self):
        return self.variables

    def _tuple_rep(self):
        return self.__class__.__name__, self.handle.term_ref, self.variables


@simplify.define(EnterCompiledCode)
def simplify_enter_compiled_code(self, frame):
    mode = tuple(v.isBound(frame) for v in self.variables)
    expr = self.handle.compiled_expressions.get(mode)
    if expr is not None:
        # then we have found something so we are going to run it and return the result
        arguments = tuple(v.getValue(frame) if v.isBound(frame) else None for v in self.variables)
        result = expr.execute_program(arguments)

        for var, val, omode in zip(self.variables, result, expr.outgoing_mode):
            if omode:
                var.setValue(frame, val)

        return expr.R  # this needs to rename the additional variables

        assert False  # TODO: load the results back into the output

    return self


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


class CompiledInstance:
    """
    Wrap a compile task so that everything has access to the right operators
    """

    def __init__(self, R, incoming_mode):
        self.operations = []  # List[Tuple[RBaseType,EvalFunction (if any)]]
        self.R = R
        self.origional_R = R
        self.bound_variables = dict((v, False) for v in set(R.all_vars()))
        self.frame_variables = [v._VariableId__name for v in self.bound_variables.keys()]  ####################################### BAD USING THE PRIVATE NAME, NEED TO FIX THIS AS AN API OR SOMETHING
        #self.exposed_vars = exposed_vars
        self.incoming_mode = incoming_mode
        self.outgoing_mode = incoming_mode
        for i, imode in enumerate(incoming_mode):
            v = VariableId(i)  # the names of the exposed variables should be normalized to just 0,...,N
            assert v in self.bound_variables
            if imode:
                self.bound_variables[v] = True

    def compile_simplfiy(self, R):
        # this is going to compile a single round of simplify, looking for
        # expressions that are able to run.  To emulate the entire saturate
        # call, this should be called in a loop until there is nothing left for
        # it to be able to compile.

        def rewriter(R):
            if isinstance(R, FinalState):
                return R  # there is nothing for us to rewrite
            if isinstance(R, Intersect):
                return R.rewrite(rewriter)
            elif isinstance(R, Partition):
                raise NotImplementedError()  # TODO: handle partition, I suppose
                                             # that this should just simplify
                                             # the children.  Which means that
                                             # this needs to call on the
                                             # children branches and evaluate
                                             # the different expressions.
            elif isinstance(R, CompiledCallTerm):
                # then we are going to require handling.  If the mode is
                # present, then we /could/ call it, or we could delay calling
                # this until we evaluate more of the delayed constraints

                # if the expression can be called and would return the correct


                raise NotImplementedError()
                pass
            else:
                out_mode = abstract_outmodes(R, self)
                if out_mode:
                    # then there is something that we can run here, so we should
                    # just mark it as running and then return the result

                    out_r, bound_variables, evaluate = out_mode[0]

                    for v in bound_variables:
                        assert v in self.bound_variables  # ensure that we don't add to this
                        self.bound_variables[v] = True

                    self.operations.append(('run_function', evaluate))
                    return out_r

                # then we can not run this expression this should maybe try
                # and perform a rewrite on its children?  That would be
                # similar to what simplify would do in these case,
                return R.rewrite(rewriter)

        return rewriter(R)

    def compile_loop(self, R):
        # take an aggregator and then loop over the elements.
        pass


    def identify_runnable_partitions(R):
        # identify where there are iterators and we could run the expression.
        # This should be if there are some moded operations that can bind a
        # variable, then we would like to know about that?

        pass

    def start_compiler(self):
        # this should try and emulate the simplify and then loop strategy to try
        # and ground out expressions? Or should loop only be used inside of an
        # aggregator, and thus there would be a well defined constract for what
        # the shape of the returned valeus would look like

        R = self.R

        while True:
            last_R = R
            R = self.compile_simplfiy(R)
            if last_R == R:
                break

        self.outgoing_mode = tuple(self.bound_variables[VariableId(i)] for i in range(len(self.incoming_mode)))
        # I suppose that we should save this back?  This is the returned expression that we are going to have to handle.
        self.R = R

    def execute_program(self, arguments):
        # this is the "bytecode interpeter" of the compiled sequence.  This is
        # just because we are compiling a sequence of instructions instead being
        # some compiled external thunk

        # setup initial registers for this method
        pc = 0  # the program counter
        ninstrs = len(self.operations)
        frame = CompiledFrame(self.frame_variables)

        # load in the arguments for this expression
        for vid, (imode, val) in enumerate(zip(self.incoming_mode, arguments)):
            if imode:
                assert val is not None
                VariableId(vid).setValue(frame, val)

        # run
        while pc < ninstrs:  # if we fall off the edge, then we should be done, but maybe we should have some final instruction which tracks this instead?
            instr, data = self.operations[pc]
            if instr == 'run_function':
                # this is currently run builtin and run external as we are just wrapping that up into a python function that does the work internally
                success = data(frame)
                assert success == True  # need to handle failure cases, or where we find ourselves branching to a different case becasue of a difference in values
                pc += 1
                continue
            elif instr == 'jump':
                pc = data
                continue

            elif instr == 'iterator_make':
                # take something that we are going to iterate over and save it
                # to some slot.  This will then set the
                pass
            elif instr == 'iterator_next':
                iterator_slot, out_slot, end_of_loop = data

                pass
            elif instr == 'aggregator_init':
                pass
            elif instr == 'aggregator_add':
                pass
            elif instr == 'aggregator_finalize':
                pass


            # we are still going to need something to handle running an
            # expression that is going to need to branch and control if some
            # expression is active.  The current set of instructions is

            raise NotImplementedError()  # as the not implements currently fall through


        # this is the returned values that are bound by the expression.  This should instead
        out_values = tuple(VariableId(vid).getValue(frame) for vid in range(len(self.outgoing_mode)))
        return out_values






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

    Rp = R

    R = replace_calls(R)  # all of the calls in the method will now be replace
                          # with referneces to other compiled object

    # normalize the expression such that we don't have to handle
    exposed_vars = ce.variable_order
    vm = dict((v,VariableId(i)) for i,v in enumerate(exposed_vars))
    R, vrenames = R.rename_vars_unique(vm.get).weak_equiv(ignored=tuple(vm.values()))

    assumptions = set()
    R = remove_all_assumptions(R, assumptions.add)


    # this will wrap the compiled code.  Atm this stays around, though in the
    # future I would suspect that the thing that does the generation of code and
    # the that compiles would be two different classes with the later generating
    # the first.
    manager = CompiledInstance(R, incoming_mode)
    manager.start_compiler()

    # there needs to be some handling in the case that we are not successful

    assert isinstance(manager.R, Terminal)  # for now just check that we reach the last final state


    # save this expression back, as we might want to put some marker here so
    # that we know that we are compiling this expression.  This would mean that
    # we want to be able to later mark that it is done.
    ce.compiled_expressions[incoming_mode] = manager

    return manager


    # import ipdb; ipdb.set_trace()




    # return None  # indicating that there was some failure or that we are unable
    #              # to do this at this time.
