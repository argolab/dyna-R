from typing import *

from .interpreter import *
from .terms import CallTerm, BuildStructure, Evaluate, ReflectStructure, Evaluate_reflect
from .guards import remove_all_assumptions


class IdentityKey:
    __slots__ = ('key',)
    def __init__(self, key):
        assert not isinstance(key, IdentityKey)
        self.key = key
    def __eq__(self, other):
        return type(self) is type(other) and self.key is other.key
    def __hash__(self):
        return object.__hash__(self.key)
    def __repr__(self):
        return f'Identity({self.key})'


# abstract information about what modes are going to come back from an expression and how
abstract_outmodes = Visitor(track_source=False)

@abstract_outmodes.default
def abstract_outmodes_default(R, bound):
    raise NotImplementedError()

@abstract_outmodes.define(ModedOp)
def abstract_outmodes_modedop(self, manager: 'CompileManager'):
    mode = tuple(v.isBound(manager) for v in self.vars)
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

            for var, val, imode in zip(self.vars, r, mode):
                # this does not raise a unification failure, as it is just going to directly write the value into the frame
                if imode:
                    if var.getValue(frame) != val:
                        return False
                else:
                    var.rawSetValue(frame, val)

            return True  # indicate that this was successful
        return  [
            # what it rewrites as, what would become bound, some function that needs to be evaluated
            (True, Terminal(1), self.vars, ev),
            # there could be more expressions here, but these do not necessary
        ]
    if mode in self.nondet:
        # then we want to be able to iterate these variables, meaning that we should return that
        f = self.nondet[mode]

        binding_vars = tuple(v for imode, v in zip(mode, self.vars) if not imode)

        # def run_iterator(frame):
        #     # then this creates an iterator and will perform modifications to
        #     # the frame in place
        #     #
        #     # I don't think that this needs to handle the binding case, as it
        #     # could just use the normal execution of a subgoal to check the
        #     # grounding of a variable

        #     vals = tuple(v.getValue(frame) for v in self.vars)
        #     r = f(*vals)

        #     # first check any ground variables are matched with the returned expression
        #     for ival, rval, imode in zip(vals, r, mode):
        #         if imode:
        #             if hasattr(rval, '__iter__'):
        #                 if ival not in rval:
        #                     return
        #             else:
        #                 if ival != rval:
        #                     return

        #     from itertools import product
        #     iterator = product(*(p if hasattr(p, '__iter__') else [p] for p, m in zip(r, mode) if not m))  # get the product of all of the iterators that are getting bound, this is a bit different from the way this is normally implemented....

        #     for binding in iterator:
        #         for var, val in zip(binding_vars, binding):
        #             var.rawSetValue(frame, val)
        #         yield  # let our caller get to the next state

        #     # unset the variables.  Not strictly required, but will ensure that
        #     # we don't use an unset value after the loop is done while debugging
        #     for var in binding_vars:
        #         var.rawSetValue(frame, None)


        # this should just get the iterator, and

        def get_iterator(frame):
            vals = tuple(v.getValue(frame) for v in self.vars)
            r = f(*vals)

            assert r != () and not isinstance(r, FinalState)

            for var, val, imode in zip(self.vars, r, mode):
                if hasattr(val, '__iter__'):
                    yield IteratorFromIterable(var, val)
                elif not imode:
                    yield SingleIterator(var, val)

        assert len(binding_vars) == 1  # this should generate different expressions for the variable that it is binding

        return [
            # return that this is non-det and which variables are going to be bound as a result of this expression
            (False, Terminal(1), binding_vars, get_iterator)
        ]

    return None  # indicates that there is nothing that we can do here


@abstract_outmodes.define(Unify)
def abstract_outmodes_unify(self, manager):
    if self.v1.isBound(manager) and self.v2.isBound(manager):
        # then we are just going to check equlaity
        def check_equals(frame):
            return self.v1.getValue(frame) == self.v2.getValue(frame)

        return [
            (True, Terminal(1), (), check_equals)
        ]
    else:
        a, b = self.v1, self.v2
        if b.isBound(manager):
            a,b = b,a
        if a.isBound(manager):
            def copy_var(frame):
                b.rawSetValue(frame, a.getValue(frame))
                return True  # this doesn't check anything as it isn't bound
            return [
                (True, Terminal(1), (b,), copy_var)
            ]

@abstract_outmodes.define(BuildStructure)
def abstract_outmodes_buildstructure(self, manager):
    amode = tuple(v.isBound(manager) for v in self.arguments)
    if self.result.isBound(manager):
        if all(amode):
            # then just generate some equality check between the two
            pass
        else:
            pass
        assert False  # TODO
    elif all(amode):
        # then build this structure and set it to the result
        assert False  # TODO


def make_interpreter_iterator_to_compiler(iterator, frame):
    # This is a hack, should be removed?  Ideally we know which expressions are coming back, and which branches can be disabled
    for result in iterator.run(frame):
        for var, val in result.items():
            var.rawSetValue(frame, val)
        yield  # let the iterator get the value


####################################################################################################


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
        raise Exception('Should not use setValue on the frame inside of the compiled code, instead use var.rawSetValue')
        # key = self._vmap[varname]
        # self._values[key] = value

    def __repr__(self):
        nice = {str(k).split('\n')[0]:self._values[v] for k,v in self._vmap.items()}
        import pprint
        return pprint.pformat(nice, indent=1)


class CompiledVariable(Variable):
    """This should know which slot it is contained in.  this means that there
    should be some frame descriptor like object that it could use to track that
    the version is correct.  the frame itself then just becoems an array

    """
    def __init__(self):
        assert False  # TODO

    pass

####################################################################################################


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

        # first save the result of the public variables that we can read

        # zip will stop early if any of the iterators stop
        for var, val, omode in zip(self.variables, result, expr.outgoing_mode):
            if omode:
                var.rawSetValue(frame, val)

        nvars = len(self.variables)
        nvm = {VariableId(i): v for i,v in enumerate(self.variables)}  # make the public variables have the same name

        # handle private variables that need new slots in this frame
        for var, val, omode in zip(expr.outgoing_additional_variables, result[nvars:], expr.outgoing_mode[nvars:]):
            v = VariableId()  # create a new variable name that is unique for this frame
            nvm[var] = v
            if omode:
                v.rawSetValue(frame, val)

        return expr.R.rename_vars(lambda v: nvm[v] if not isinstance(v, ConstantVariable) else v)  # there should not be variables that we are unaware of

        # return expr.R  # this needs to rename the additional variables

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


# class CompiledModedCall(RBaseType):

#     def __init__(self, dyna_system, term_ref, call_mode :Tuple[bool], arguments :Tuple[Variable]):
#         self.dyna_system = dyna_system
#         self.term_ref = term_ref
#         self.call_mode = call_mode
#         self.result_mode = None  # TODO: need to know what is bound after this call is performed, and we are going to need to still plan which expressions in the return bit are still to get handled.
#         self.arguments = arguments

#         self.returned_rexpr = None  # TODO: need to know what could possibly come back in this case

#     @property
#     def vars(self):
#         return self.arguments



def replace_calls(R):
    if isinstance(R, CallTerm):
        return CompiledCallTerm(R.var_map, R.dyna_system, R.term_ref,
                                R.dyna_system.create_compiled_expression(R.term_ref, R.var_map.values()))
    if isinstance(R, (Evaluate, Evaluate_reflect, ReflectStructure)):
        raise RuntimeError('unable to compile this expression, it uses reflection, try the optimizer first')  # TODO: should be some typed error that we can throw
    return R.rewrite(replace_calls)



class CompiledPartition(RBaseType):

    def __init__(self, unioned_vars :Tuple[VariableId], children :List[Tuple[Tuple[object],int,RBaseType]]):
        super().__init__()
        self._unioned_vars = unioned_vars
        self._children = children

    def rename_vars(self, remap):
        r = tuple(remap(v) for v in self._unioned_vars)
        assert not any(isinstance(v, ConstantVariable) for v in r)  # TODO: handle this case
        return CompiledPartition(r, [(a,b,c.rename_vars(remap)) for a,b,c in self._children])

    @property
    def vars(self):
        return self._unioned_vars + tuple(v for c in self._children for v in c[0] if v is not None)

    @property
    def children(self):
        return tuple(c for _,_,c in self._children)

@simplify.define(CompiledPartition)
def simplify_compiled_partition(self, frame, **kwargs):
    # this is running in the interpreter on the result of a compiled partition.
    # This should return a /normal/ partition which uses the information that
    # was tracked statically about what elements of the partition were rewritten

    nc = PrefixTrie(len(self._unioned_vars))
    # this should just loop over the different branches of the partition and
    # convert each one into a branch in the "normal" partition.  Then just call
    # that to perform its simplification

    for local_vars, failure_id, body in self._children:
        # this needs to get the failure ID and determine if this branch has actually failed, or if it would just have

        values = tuple(v.getValue(frame) for v in local_vars)
        # this can either just use the local vars with unification or it could
        # perform a rename on the body of the expression.  With the rename, it
        # would be easier to detect iteration possibilityies (atm).

        # this should do the rename back to the origional variable names, that
        # will allow it to delete the additional variables out of the frame as
        # well, which can be helpful in the interpreter to try and /not/ polute
        # the namespace too much.


        pass


    raise NotImplementedError()

    nr = Partition(self._unioned_vars, nc)
    return simplify(nr, frame, **kwargs)


class CheckEqualConstant(RBaseType):

    def __init__(self, constant: object, variable: VariableId):
        super().__init__()
        self.constant = constant
        self.variable = variable

    @property
    def vars(self):
        return self.variable,

    def rename_vars(self, remap):
        v = remap(self,variable)
        if v == self.variable: return self
        if isinstance(v, ConstantVariable):
            if v.getValue(None) == self.constant:  # then the check can be done statically so we do that
                return Terminal(1)
            else:
                return Terminal(0)
        return CheckEqualConstant(self.constant, v)

@abstract_outmodes.define(CheckEqualConstant)
def abstract_outmodes_checkequals(self, manager):
    raise NotImplementedError()  # TODO: should make the variable iterable as a single value and also be able to check that the value is equal


def replace_partitions(R):
    counter = 1
    def rewriter(R):
        nonlocal counter
        if isinstance(R, Partition):
            children = []
            for kk, vv in R._children.items():
                assert not any(isinstance(k, ConstantVariable) for k in kk)  # TODO: this needs to be handled by rewriting as unifications and embedding constant values into the program
                for v in vv:
                    # need new variable names for this operation.  Then we can have that
                    rpv = tuple(VariableId() if k is None else None for k in kk)

                    consts = []
                    for u, k in zip(R._unioned_vars, kk):
                        if k is not None:
                            # this shouldn't set a variable equal to a value,
                            # but should be usable for iteration and checking
                            # equality with this constant
                            consts.append(CheckEqualConstant(k, u))
                            #consts.append(Unify(constant(k), u)  # check that these variables are consistent with the value that

                            assert False  # TODO: make this work

                    c = counter
                    counter += 1

                    rm = dict((k,r or v) for k,r,v in zip(R._unioned_vars, rpv, kk))
                    v = v.rename_vars(lambda x: rm.get(x,x))  # rename the variables to get new slots
                    #assert None not in v.all_vars()

                    if not v.isEmpty():  # ignore stuff that we somehow manage to delete?
                        children.append((rpv, c, rewriter(v)))

            # TODO: this also should check if for one of the variables that it
            # is the same value across all branches, in which case that should
            # get propagated up.

            return CompiledPartition(R._unioned_vars, children)
        else:
            return R.rewrite(rewriter)
    return rewriter(R)


####################################################################################################


class CompiledInstance:
    """
    Wrap a compile task so that everything has access to the right operators
    """

    def __init__(self, R, incoming_mode):
        self.operations = []  # List[Tuple[RBaseType,EvalFunction (if any)]]
        self.R = R
        self.origional_R = R

        # TODO: the variables should just be those that we actually set at some
        # point in time.  It is possible that there are extra variables that are
        # never used (such on branches of partitions that are disabled) and in
        # those cases we would like to ignore the resulting values
        self.bound_variables = dict((v._compiler_name, False) for v in set(R.all_vars()) if not isinstance(v, ConstantVariable))
        self.frame_variables = list(self.bound_variables.keys())  ####################################### BAD USING THE PRIVATE NAME, NEED TO FIX THIS AS AN API OR SOMETHING
        #self.exposed_vars = exposed_vars

        self.incoming_mode = incoming_mode
        self.outgoing_mode = None
        self.outgoing_additional_variables = [] # list of variables that will be added to the frame, these are appended to the list of the result
        for i, imode in enumerate(incoming_mode):
            #v = VariableId(i)  # the names of the exposed variables should be normalized to just 0,...,N
            assert i in self.bound_variables
            if imode:
                self.bound_variables[i] = True
        self.failure_handler_instruction = -1

    def _frame_isbound(self, varname):
        return self.bound_variables[varname]

    def _add_variable(self, variable):
        assert variable not in self.bound_variables
        self.bound_variables[variable._compiler_name] = False
        self.frame_variables.append(variable._compiler_name)

    def _operation_add(self, operation):
        pc = len(self.operations)
        self.operations.append(operation)
        return pc

    @property
    def _operation_pc(self):
        return len(self.operations)

    def finalize_compiler(self):
        in_vars_out = tuple(self.bound_variables[i] for i in range(len(self.incoming_mode)))

        # these are additional variables that we are going to return as being in the out inst
        additional_vars = list(set(v for v in self.R.all_vars() if not isinstance(v, ConstantVariable)) - set(VariableId(i) for i in range(len(self.incoming_mode))) - set(self.outgoing_additional_variables))

        # if we finalize this more than once then we should continue to add in
        # the variables.  So we might expose more variables then we actually
        # want in that case?

        self.outgoing_additional_variables += additional_vars

        additional_mode = tuple(self.bound_variables[v._compiler_name] for v in self.outgoing_additional_variables)
        self.outgoing_mode = in_vars_out + additional_mode
        # I suppose that we should save this back?  This is the returned expression that we are going to have to handle.

    def do_compilation(self):
        self.R = self.compile_saturate(self.R)
        self.finalize_compiler()

    def do_compilation_nondet(self):
        # generated a version of the code where this yields all of the values
        # one at a time rather than returning a single R-expr that represents
        # what would be the partition that is returned.
        #
        # the reason for this method is wihtout it, we might not be able to make
        # that much progress.  Expanding the partition into all of its ground
        # values might be what we are actually interested in.  In which case we
        # might have different possible returned expressions that need to be handled.

        # we might only want to iterate binding a single variable and then have
        # a different bit of generated code which could handle the result of
        # that.  That way we could deal with information being passed both ways
        # like in the interpeter

        pass


    def compile_simplfiy(self, R, *, replaced_expressions=None):
        # this is going to compile a single round of simplify, looking for
        # expressions that are able to run.  To emulate the entire saturate
        # call, this should be called in a loop until there is nothing left for
        # it to be able to compile.

        # if nondet_runners is None:
        #     nondet_runners = {}
        if replaced_expressions is None:
            replaced_expressions = {}

        def rewriter(R):
            nonlocal replaced_expressions
            if IdentityKey(R) in replaced_expressions:
                return replaced_expressions.pop(IdentityKey(R))
            elif isinstance(R, FinalState):
                return R  # there is nothing for us to rewrite
            elif isinstance(R, Intersect):
                return R.rewrite(rewriter)
            elif isinstance(R, Partition):
                assert False  # should never happen.  partitions need to be replaced by CompiledPartitions first
            elif isinstance(R, CompiledPartition):
                result_children = []
                parent_failure = self.failure_handler_instruction
                incoming_mode = tuple(v.isBound(self) for v in R._unioned_vars)
                try:
                    # any variable that is bound, should just be deleted from the partition, and we can just focus on things that are non-ground
                    all_ground = all(incoming_mode)  # we can use a simpler failure handler as there will be nothing left for us to track after this point
                    for (kk, pid, v) in R._children:
                        assert not any(isinstance(k, ConstantVariable) for k in kk)  # TODO: ??? I am not sure if this could happen at this point
                        try:
                            failure_pc = self._operation_pc
                            self._operation_add(('PLACE HOLDER failure_handler_conditional_run', '__FILL_IN__', pid))

                            # do the generation for this branch.  If there are
                            # constants than we need to check if those are set?
                            # The constants should be embedded into the branch.
                            # That should be something that we can do before we
                            # get to this point in compilation.

                            var_renames = {lvar: uvar for imode, uvar, lvar in zip(incoming_mode, R._unioned_vars, kk) if imode and lvar is not None}

                            if var_renames:
                                v = v.rename_vars(lambda x: var_renames.get(x,x))

                            #import ipdb; ipdb.set_trace()
                            res = rewriter(v)

                            kkr = tuple(k for k, imode in zip(kk, incoming_mode) if not imode)
                            result_children.append((kkr, pid, v))
                        finally:
                            if all_ground:
                                pass
                            # then this should only be used in the case that we need to track this for another step
                            self.operations[failure_pc] = ('failure_handler_conditional_run', self._operation_pc, pid)

                finally:
                    self._operation_add(('failure_handler_jump', parent_failure))
                    self.failure_handler_instruction = parent_failure

                uvr = tuple(v for v, imode in zip(R._unioned_vars, incoming_mode) if not imode)
                return CompiledPartition(uvr, result_children)

            elif isinstance(R, CompiledCallTerm):
                # then we are going to require handling.  If the mode is
                # present, then we /could/ call it, or we could delay calling
                # this until we evaluate more of the delayed constraints

                # if the expression can be called and would return the correct


                raise NotImplementedError()

            elif isinstance(R, Aggregator):
                # the aggregator might create a loop if the body is finished

                #parent_nondets = nondet_runners
                #local_nondets = {}
                #nondet_runners = local_nondets
                #try:
                body = rewriter(R.body)
                #finally:
                #    nondet_runners = parent_nondets

                # this needs to check if one of the iterators could bind
                # something that is in a higher frame.  that might mean tracking
                # if there is some functional dependency between one of the
                # operators?

                if all(v.isBound(self) for v in R.head_vars):
                    # then we can run this aggregator for a specific variable, so we are going to do that now

                    # this is going to need to select something that can be used to run properly.  For now, we are just choosing the first thing
                    #assert len(local_nondets) == 1

                    aggregator_slot = VariableId()
                    self._add_variable(aggregator_slot)
                    #self.bound_variables[aggregator_slot._compiler_name] = True

                    self.operations.append(('aggregator_init', aggregator_slot))

                    runnable_parts = list(self.identify_runnable_partitions(body))

                    #loop_driver, loop_op = list(local_nondets.items())[0]  # just get the first value for now
                    loop_driver = IdentityKey(runnable_parts[0][0])
                    loop_op = runnable_parts[0][1]

                    replaced_expressions[loop_driver] = loop_op[1]

                    def aggregator_callback():
                        # this needs to generate the approperate code inside of the loop for handling the R-expr that remains.

                        # this needs to become a saturate to get it all the way to the end of running
                        rbody = rewriter(body)  # run the body and get the result

                        assert isinstance(rbody, Terminal)  # that we reached the final state

                        self.operations.append(('aggregator_add', (aggregator_slot, R.body_res, R.aggregator)))  # perform the addition into the aggregator for this operation


                    self.compile_run_loop(body, loop_op, aggregator_callback)

                    self.operations.append(('aggregator_finalize', (aggregator_slot, R.result)))
                    self.bound_variables[R.result._compiler_name] = True  # mark that the result variable is now set to something

                    return Terminal(1)

                return Aggregator(R.result, R.head_vars, R.body_res, R.aggregator, body)
            else:
                out_mode = abstract_outmodes(R, self)
                if out_mode:
                    # then there is something that we can run here, so we should
                    # just mark it as running and then return the result

                    is_semidet, out_r, bound_variables, evaluate = out_mode[0]

                    if is_semidet:
                        for v in bound_variables:
                            if not isinstance(v, ConstantVariable):
                                n = v._compiler_name
                                assert n in self.bound_variables  # ensure that we don't add to this
                                self.bound_variables[n] = True

                        self.operations.append(('run_function', evaluate))
                        return out_r
                    else:
                        pass
                        #nondet_runners[IdentityKey(R)] = out_mode[0]  # track this for something else could use this to drive a loop
                        # assert False  # then we need to mark that this oepration
                        #               # could be run in a non-det mode
                        #               # (iterating) the domain of some variable



                # then we can not run this expression this should maybe try
                # and perform a rewrite on its children?  That would be
                # similar to what simplify would do in these case,
                return R.rewrite(rewriter)

        return rewriter(R)

    def compile_run_loop(self, R, iterator, callback):
        # take an aggregator and then loop over the elements.


        _, result_r, bound_variables, run_iterator = iterator

        iter_slot = VariableId()
        self._add_variable(iter_slot)
        #self.bound_variables[iter_slot._compiler_name] = True  # this will make the compiler allocate some slot for this variable

        entry_binding_state = self.bound_variables.copy()  # we want to unset any bound variables when we return

        self.operations.append(('iterator_load_start', (run_iterator, iter_slot)))
        loop_pc = len(self.operations)
        self.operations.append(('iterator_next', (iter_slot, -1)))  # we have to fill in the jump location for when this loop is done

        parent_failure = self.failure_handler_instruction
        self.failure_handler_instruction = loop_pc

        try:
            for var in bound_variables:
                self.bound_variables[var._compiler_name] = True  # these are bound by the iterator

            callback()
        finally:
            self.operations.append(('jump', loop_pc))
            self.operations.append(('failure_handler_jump', parent_failure))  # reset the failure handling to our caller's failure handler in the generated code
            self.operations[loop_pc] = ('iterator_next', (iter_slot, len(self.operations)))

            for var, state in entry_binding_state.items():
                self.bound_variables[var] = state
            self.failure_handler_instruction = parent_failure

            #self.bound_variables[iter_slot._compiler_name] = False  # this is now unbound

    def compile_loop(self, R, callback):
        # compile having multiple loops at the same time until some criteria has been meet.

        runnable = list(self.identify_runnable_partitions(R))

        # this should choose something that it can run, and then use that.  Otherwise, we are going to have to handle the cases where


        raise NotImplementedError()


    def identify_runnable_partitions(self, R):
        # identify where there are iterators and we could run the expression.
        # This should be if there are some moded operations that can bind a
        # variable, then we would like to know about that?

        def walker(R):
            if isinstance(R, CompiledPartition):
                children_iterable = {}

                # this should map to the variables that are bound.  In which case, this will want to handle if there are differences.
                for vs, pid, c in R._children:
                    for r, info in walker(c):
                        is_semidet, out, bound_variables, evaluate = info
                        for var in bound_variables:
                            # if the var is is in the constants, then it should
                            # have that these variables are going to just become
                            # a single iterable value.
                            children_iterable.setdefault(var, []).append((r, info))

                for i, var in enumerate(R._unioned_vars):
                    cnt_outer = False
                    iterators = []
                    for vs, pid, c in R._children:
                        ci = children_iterable.get(vs[i])
                        if ci is None:
                            cnt_outer = True
                            break
                        iterators.append(ci)
                    if cnt_outer:
                        continue



                    import ipdb; ipdb.set_trace()


                raise NotImplementedError()
            elif isinstance(R, Aggregator):
                # this needs to filter which things are reported as runnable so that we don't have
                raise NotImplementedError()
            else:
                if not isinstance(R, Intersect):
                    out_mode = abstract_outmodes(R, self)
                    if out_mode:
                        is_semidet, out, bound_variables, evaluate = out_mode[0]
                        if not is_semidet:
                            yield R, out_mode[0]
                for c in R.children:
                    yield from walker(c)

        yield from walker(R)

    def compile_saturate(self, R):
        # this should try and emulate the simplify and then loop strategy to try
        # and ground out expressions? Or should loop only be used inside of an
        # aggregator, and thus there would be a well defined constract for what
        # the shape of the returned valeus would look like

        while True:
            last_R = R
            R = self.compile_simplfiy(R)
            if last_R == R:
                break

        return R


    ##################################################

    def execute_program(self, arguments):
        # this is the "bytecode interpeter" of the compiled sequence.  This is
        # just because we are compiling a sequence of instructions instead being
        # some compiled external thunk

        # setup initial registers for this method
        pc = 0  # the program counter
        ninstrs = len(self.operations)
        frame = CompiledFrame(self.frame_variables)
        failure_handler = -1

        # this stack stuff should just become embedded as new variables rather than having its own things that are tracked?
        # then we can just put it into the frame
        failure_handler_stack = [0]  # this should just become static variables in C++
        failure_handler_condition = 0

        def fail():
            nonlocal pc, frame, failure_handler, failure_handler_stack, failure_handler_condition
            pc = failure_handler
            failure_handler_stack[-1] |= failure_handler_condition

        # load in the arguments for this expression
        for vid, (imode, val) in enumerate(zip(self.incoming_mode, arguments)):
            if imode:
                assert val is not None
                VariableId(vid).rawSetValue(frame, val)

        # run
        while pc < ninstrs:  # if we fall off the edge, then we should be done, but maybe we should have some final instruction which tracks this instead?
            assert pc >= 0
            instr, data = self.operations[pc]
            print(f'>>> {pc} {instr} {data}')
            if instr == 'run_function':
                # this is currently run builtin and run external as we are just wrapping that up into a python function that does the work internally
                success = data(frame)
                if not success:
                    fail()
                else:
                    #assert success == True  # need to handle failure cases, or where we find ourselves branching to a different case becasue of a difference in values
                    pc += 1
            elif instr == 'jump':
                pc = data
            elif instr == 'iterator_load_start':
                # take something that we are going to iterate over and save it
                # to some slot.  This will then set the
                get_iterator, iter_slot = data
                iterators = list(get_iterator(frame))
                assert len(iterators) == 1
                iter_slot.rawSetValue(frame, make_interpreter_iterator_to_compiler(iterators[0], frame))  # start the iterator
                pc += 1
            elif instr == 'iterator_start':
                iterator_construct_slot, iter_slot = data
                it = iterator_construct_slot.getValue(frame)
                if it is None:
                    # then all of the iterators failed to be constructed, this is empty
                    # we are going to need to go to some failure handler in this case
                    fail()
                else:
                    iter_slot.rawSetValue(frame, it(frame))
                pc += 1
            elif instr == 'iterator_union_make':
                # construct a union iterator taking into account which partition is currently "alive"
                run_iterator, iter_slot, condition = data

                # check the condition
                if not (failure_handler_stack[-1] & (1 << condition)):
                    # then we can add this iterator
                    it = iter_slot.getValue(frame)
                    if it is None:
                        assert False
                    elif isinstance(it, UnionIterator):
                        assert False
                    else:
                        # then build a union iterator out of these two iterators
                        assert False

                    iter_slot.rawSetValue(frame, it)
            elif instr == 'iterator_next':
                iterator_slot, end_iterator_location = data
                iterator = iterator_slot.getValue(frame)
                failure_handler = pc
                try:
                    next(iterator)  # get the next value from the iterator
                    pc += 1  # go to the next slot
                except StopIteration:
                    pc = end_iterator_location
                    iterator_slot.rawSetValue(frame, None)  # delete the iterator from the frame slot
                iterator = None
            elif instr == 'aggregator_init':
                data.rawSetValue(frame, None)  # init the value to nothing
                pc += 1
            elif instr == 'aggregator_add':
                slot, body_res, aggregator = data
                old_value = slot.getValue(frame)
                new_value = body_res.getValue(frame)
                if old_value is not None:
                    new_value = aggregator.combine(old_value, new_value)
                slot.rawSetValue(frame, new_value)
                pc += 1
            elif instr == 'aggregator_finalize':
                slot, out_var = data
                value = slot.getValue(frame)
                if value is None:
                    fail()
                    #assert False  # TODO: handle.  In this case there was nothing that got aggregated together and we need to error out this statement and go to whatever the failure handler is in this case
                else:
                    out_var.rawSetValue(frame, value)
                    pc += 1
            elif instr == 'failure_handler_jump':  # this shouldn't really be here?  I suppose that we currently need to be able to reset this instruction
                failure_handler = data
                pc += 1
            elif instr == 'failure_handler_push':
                failure_handler_stack.append(failure_handler_stack[-1])
                pc += 1
            elif instr == 'failure_handler_pop':
                del failure_handler_stack[-1]
                pc += 1
            elif instr == 'failure_handler_conditional_run':  # run the next block if it hasn't already been marked as failed, otherwise set
                next_pc, condition = data
                if failure_handler_stack[-1] & (1 << condition):
                    # then this branch is currently disabled
                    pc = next_pc
                    failure_handler = -1
                else:
                    # then this branch is not disabled, so we are going to run the code
                    failure_handler = next_pc
                    failure_handler_condition = 1 << condition  # use a bit mask for this stuff
                    pc += 1



        # this is the returned values that are bound by the expression.  This should instead
        out_values = tuple((VariableId(vid).getValue(frame) for vid in range(len(self.incoming_mode)))) + tuple((v.getValue(frame) for v in self.outgoing_additional_variables))
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
    R = replace_partitions(R)


    # this will wrap the compiled code.  Atm this stays around, though in the
    # future I would suspect that the thing that does the generation of code and
    # the that compiles would be two different classes with the later generating
    # the first.
    manager = CompiledInstance(R, incoming_mode)
    manager.do_compilation()

    # there needs to be some handling in the case that we are not successful

    #assert isinstance(manager.R, Terminal)  # for now just check that we reach the last final state


    # save this expression back, as we might want to put some marker here so
    # that we know that we are compiling this expression.  This would mean that
    # we want to be able to later mark that it is done.
    ce.compiled_expressions[incoming_mode] = manager

    return manager
