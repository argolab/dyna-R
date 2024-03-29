#from functools import reduce
#import operator

from .exceptions import *
from .interpreter import *
from .optimize import optimizer

def _term_op(op):
    def oper(*args):
        # this does not have the current reference to the dyna_system
        # this makes this somewhat brittle in the case that new things were defined in a different dyna instance
        from . import dyna_system
        return dyna_system.raw_call('op_'+op, args)
    return oper

def _builtin_eq(a,b):
    if isinstance(a, Term):
        return a.builtin_eq(b)
    elif isinstance(b, Term):
        return False
    return a == b

class Term:
    # This should probably be renamed from "term" to "named tuple" or something
    # "term" is just overused in the system and there are other values that we
    # can represent in the system

    __slots__ = ('__name', '__arguments', '__hashcache')

    def __init__(self, name, arguments):
        self.__name = name
        assert all(not isinstance(a, Variable) for a in arguments) and isinstance(name, str)
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable
        self.__hashcache = hash(self.name) ^ hash(self.__arguments)

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments

    def builtin_eq(self, other):
        # perform an equal operation using only builtin compares
        return self is other or \
            (isinstance(other, Term) and
             hash(self) == hash(other) and
             self.name == other.name and
             len(self.arguments) == len(other.arguments) and
             all(_builtin_eq(a,b) for a,b in zip(self.arguments, other.arguments)))

    def builtin_lt(self, other):
        if not isinstance(other, Term):
            return False
        a = self.__hashcache
        b = other.__hashcache
        if a == b and self != other:
            # there needs to be some order on these element
            if self.name != other.name:
                return self.name < other.name
            if len(self.arguments) != len(other.arguments):
                return len(self.arguments) < len(self.arguments)
            for a,b in zip(self.arguments, other.arguments):
                if a != b:
                    return a < b
            assert False  # these are not equal, but could not identify where these are not equal
        return a < b  # we just need some order on these

    def __eq__(self, other):
        # maybe just make it so equal is not overrideable?
        # this is just term equality comparing between the values
        return self.builtin_eq(other)

    # def __lt__(self, other):
    #     assert False
    #     return self.builtin_lt(other)

    # these should be automatically defined by python
    # def __ne__(self, other):
    #     return not (self == other)

    # def __gt__(self, other):
    #     return other < self

    def __hash__(self):
        return self.__hashcache

    # convert between the dyna linked list version of a list and python's list
    def aslist(self):
        if self.__name == '.' and len(self.__arguments) == 2:
            return [self.__arguments[0]] + self.__arguments[1].aslist()
        elif self.__name == 'nil' and len(self.__arguments) == 0:
            return []

    @staticmethod
    def fromlist(lst):
        if len(lst) == 0:
            return Term('nil', ())
        return Term('.', (lst[0], Term.fromlist(lst[1:])))

    # this should have operators which are defined for terms
    # in the case that there is nothing defined, then
    __add__ = _term_op('+')
    __sub__ = _term_op('-')
    __mul__ = _term_op('*')
    __div__ = _term_op('/')
    __truediv__ = _term_op('/')

    _dyna_eq = _term_op('==')
    __lt__ = _term_op('<')

    # does this need an equals and <, <= operator which are exposed to dyna?  If
    # this was to be used with an aggregator like min/max, then it would be nice
    # if those operators were able to expose those operations?


    def __str__(self):
        return f'{self.__name}({", ".join(map(str, self.__arguments))})'

    def __repr__(self):
        return str(self)



class BuildStructure(RBaseType):
    """
    Build something like X=&foo(Y).
    """

    def __init__(self, name :str, result :Variable, arguments :List[Variable]):
        super().__init__()
        self.name = name
        self.result = result
        self.arguments = tuple(arguments)

    @property
    def vars(self):
        return (self.result, *self.arguments)

    def rename_vars(self, remap):
        res = remap(self.result)
        args = [remap(a) for a in self.arguments]
        if res in args:  # occurs check at a single level, so something like `X=s(X)`
            return Terminal(0)
        return BuildStructure(self.name, res, args)

    def _tuple_rep(self):
        return self.__class__.__name__, self.name, self.result, self.arguments


@simplify.define(BuildStructure)
def simplify_buildStructure(self, frame):
    if self.result.isBound(frame):
        # then the result variable is bound, so we are going to unpack it and assign it to the variables
        res = self.result.getValue(frame)
        if not isinstance(res, Term) or res.name != self.name or len(res.arguments) != len(self.arguments):
            return Terminal(0)  # then this has failed
        # import inspect
        # #and str(self.arguments[0]) != '$V3':#
        # if self.name == 'inp' and not any(('(c, frame2)' in f.code_context[0] if f.code_context else False) for f in inspect.stack()):
        #     import ipdb; ipdb.set_trace()

        # if self.name == 'out':
        #     import ipdb; ipdb.set_trace()

        for var, val in zip(self.arguments, res.arguments):
            var.setValue(frame, val)
        return Terminal(1)
    elif all(v.isBound(frame) for v in self.arguments):
        # then the result must not be bound, so we are just going to construct it
        res = Term(self.name, [v.getValue(frame) for v in self.arguments])
        self.result.setValue(frame, res)
        return Terminal(1)
    else:
        # set type information in the frame so that this can be tracked more easily
        self.result.setType(frame, (self.name, len(self.arguments)))

    return self


@getPartitions.define(BuildStructure)
def getPartitions_buildStructure(self, frame, include_structure=False):
    if include_structure:
        yield self


@optimizer.define(BuildStructure)
def optimizer_buildStructure(self, info):

    ac = info.all_constraints[self.result]

    if len(ac) == 1 and not self.result.isBound(info.frame) and self.result not in info.exposed_variables:
        #assert not self.result.isBound(info.frame)
        assert ac[0] is self
        assert info.conjunctive_constraints[self.result] == ac
        # then we are the only constraint that is this result variable, so we
        # are just going to delete ourselves as we are not needed anymore

        return Terminal(1)

    cc = info.conjunctive_constraints[self.result]

    for c in cc:
        if c is self:
            break

        # we can only do this if we are not in a partition
        # as otherwise the partition has to be made aware of the fact that we are reading and setting these additional variables.
        if isinstance(c, BuildStructure) and c.result == self.result:  # only unified if the same result variable
            # this should unify the variables that are arguments, and just delete itself
            if c.name != self.name or len(c.arguments) != len(self.arguments):
                return Terminal(0)  # unification fails in this case

    for c in info.partition_constraints[self.result]:
        if c is self:
            break
        if isinstance(c, BuildStructure) and c.result == self.result:
            # if we are in a partition, then we would require that all of the
            # arguments of one side are present, otherwise this isn't giong to
            # work?
            #
            # This requires that both of the constraints are in the same
            # partition, otherwise this is not correct.  So we are giong to need
            # to map the partition's constraints?
            #import ipdb; ipdb.set_trace()
            const = []#unify(c.result, self.result)]
            for a,b in zip(c.arguments, self.arguments):
                const.append(unify(a,b))
            if TRACK_CONSTRUCTED_FROM:
                for u in const: u._constructed_from = self
            return intersect(*const)

    # the occurs check
    if self.result in buildStructure_determine_constructed_from(self.result, info, {}):
        # the occurs check fails
        return Terminal(0)

    return self


def buildStructure_determine_constructed_from(variable, info, mapping):
    if isinstance(variable, ConstantVariable):
        return set()
    if variable in mapping:
        return mapping[variable]
    mapping[variable] = set((variable,))
    ns = set()
    for c in info.conjunctive_constraints[variable]:
        if isinstance(c, BuildStructure) and c.result == variable:
            for a in c.arguments:
                ns |= buildStructure_determine_constructed_from(a, info, mapping)
    mapping[variable] = ns
    return ns



class ReflectStructure(RBaseType):
    """
    For reflecting the type of the quoted object with the name as a string and the body as a list of cons cells

    This should rewrite as BuildStructure as early as possible.
    So if name is a known constant and the body is a fully formed list that we can walk abstractly.

    If the lenght was known as a variable, then it might be easier to perform the rewrite?
    In which case it wouldn't have to walk the constraints that are unevaluated to determine what the length is?

    But having the length as an additional variable is not necessary in the case that
    """

    def __init__(self, result: Variable, name :Variable, num_args :Variable, args_list :Variable):
        super().__init__()
        self.result = result  # the resulting variable that we are trying to reflect
        self.name = name  # the variable that is going to take on the string value for the name
        self.num_args = num_args  # the number of arguments (length of the list), will let us rewrite in the case that not fully ground
        self.args_list = args_list  # the list of arguments that are found

    @property
    def vars(self):
        return (self.result, self.name, self.num_args, self.args_list)

    def rename_vars(self, remap):
        return ReflectStructure(
            remap(self.result),
            remap(self.name),
            remap(self.num_args),
            remap(self.args_list)
        )

    def _tuple_rep(self):
        return self.__class__.__name__, self.result, self.name, self.num_args, self.args_list

def reflect_buildMatch(self, name, num_args):
    if not isinstance(name, str) or not isinstance(num_args, int):
        return Terminal(0)

    arg_vars = [VariableId(('reflected', object())) for _ in range(num_args)]
    consts = [BuildStructure(name, self.result, arg_vars)]
    # have to construct a list constraints out of these variables
    prev = constant(Term('nil', ()))  # the end of the list
    for v in reversed(arg_vars):
        np = VariableId(('reflected_list', object()))
        c = BuildStructure('.', np, (v, prev))
        consts.append(c)
        prev = np
    consts.append(Unify(prev, self.args_list))  # this should just rewrite rather than adding in this additional constraint, but it should be fine...
    if TRACK_CONSTRUCTED_FROM:
        for c in consts: c._constructed_from = self

    R = Intersect(tuple(consts))
    return R


@simplify.define(ReflectStructure)
def simplify_reflectStructure(self, frame):
    if self.result.isBound(frame):
        res = self.result.getValue(frame)
        if not isinstance(res, Term):
            return Terminal(0)  # maybe these should be errors instead of unification failures
        self.name.setValue(frame, res.name)
        self.args_list.setValue(frame, Term.fromlist(res.arguments))
        self.num_args.setValue(frame, len(res.arguments))
        return Terminal(1)

    typ = self.result.getType(frame)

    if self.name.isBound(frame):
        name = self.name.getValue(frame)
    elif typ is not None:
        name = typ[0]
    else:
        name = None
    if self.num_args.isBound(frame):
        nargs = self.num_args.getValue(frame)
    elif typ is not None:
        nargs = typ[1]
    else:
        nargs = None

    if name is not None and self.args_list.isBound(frame):
        # then we are going to be able to construct this object.  so we are
        # going to have to walk the list and convert it back into something that we want?
        args = self.args_list.getValue(frame)
        if not isinstance(name, str) or not isinstance(args, Term):
            return Terminal(0)
        try:
            args = args.aslist()  # this might type error in the case that later down the list this doesn't form a list
        except TypeError:
            return Terminal(0)
        if args is None:
            return Terminal(0)
        res = Term(name, args)
        self.name.setValue(frame, name)
        self.num_args.setValue(frame, len(res.arguments))
        self.result.setValue(frame, res)
        return Terminal(1)

    if name is not None and nargs is not None:
        assert not self.args_list.isBound(frame)

        self.name.setValue(frame, name)
        self.num_args.setValue(frame, nargs)

        R = reflect_buildMatch(self, name, nargs)
        return simplify(R, frame)

    return self

@optimizer.define(ReflectStructure)
def optimizer_reflectstructure(self, info):
    # this should check if there are other conjunctive constraints that contain
    # the type info, and then we can use those to rewrite this constraint to not exist

    # the only case that we have to handle here is when the type of the head
    # variable is know, otherwise this can be handled via the

    cc = info.conjunctive_constraints[self.result]
    for c in cc:
        if isinstance(c, BuildStructure) and c.result == self.result:
            # if there are more than 2 conjunctive constraints with the same name, then there will be some unification failure at some point, which is ok I suppose?
            self.name.setValue(info.frame, c.name)
            self.num_args.setValue(info.frame, len(c.arguments))
            return reflect_buildMatch(self, c.name, len(c.arguments))

    # if the result or all of the argument variables are only referneced once, then
    # this should just delete like the build arguments also do
    if (len(info.all_constraints[self.result]) == 1 and
        not self.result.isBound(info.frame) and
        self.result not in info.exposed_variables):
        assert info.all_constraints[self.result] is self
        return Terminal(1)


    # we might also want to do something about the arguments list lengths in this case?

    return self



# XXX: should just delete and use the other Evaluate
class Evaluate_reflect(RBaseType):
    """
    This should completement the reflect structure operator in that if we know the name and number of arguments then we can resolve the call
    without haivng to know the all of the arguments as ground.  To construct an `*X` operator then we can combine this with reflect structure
    so that both of these operators will get rewritten together

    This is specific to dyna, as we are having the fact that there are positional arguments and that those are mapped in the tuple
    """

    def __init__(self, dyna_system, ret :Variable, name :Variable, nargs :Variable, args_list :Variable):
        super().__init__()
        self.ret = ret
        self.name = name
        self.nargs = nargs
        self.args_list = args_list
        self.dyna_system = dyna_system

    @property
    def vars(self):
        return self.ret, self.name, self.nargs, self.args_list

    def rename_vars(self, remap):
        return Evaluate_reflect(self.dyna_system, remap(self.ret), remap(self.name), remap(self.nargs), remap(self.args_list))

    def _tuple_rep(self):
        return self.__class__.__name__, self.ret, self.name, self.nargs, self.args_list

@simplify.define(Evaluate_reflect)
def simplify_evaluate_reflect(self, frame):
    if self.args_list.isBound(frame) and not self.nargs.isBound(frame):
        # then we are going to compute the length
        args = self.args_list.getValue(frame)
        args = args.aslist()  # TODO: catch an errors here???
        self.nargs.setValue(frame, len(args))
    if self.name.isBound(frame) and self.nargs.isBound(frame):
        name = self.name.getValue(frame)
        nargs = self.nargs.getValue(frame)

        if not isinstance(name, str) or not isinstance(nargs, int):
            return Terminal(0)  # then this failed, maybe this should instead be an error

        arg_vars = [VariableId(('reflected_eval', object())) for _ in range(nargs)]
        zmap = dict(zip(variables_named(*range(nargs)), arg_vars))
        zmap[ret_variable] = self.ret
        consts = [CallTerm(zmap, self.dyna_system, (name, nargs))]  # the call to the new term
        # have to construct a list constraints out of these variables
        prev = constant(Term('nil', ()))  # the end of the list
        for v in reversed(arg_vars):
            np = VariableId(('reflected_elist', object()))
            c = BuildStructure('.', np, (v, prev))
            consts.append(c)
            prev = np
        consts.append(Unify(prev, self.args_list))  # this should just rewrite rather than adding in this additional constraint, but it should be fine...

        if TRACK_CONSTRUCTED_FROM:
            for c in consts: c._constructed_from = self

        R = Intersect(tuple(consts))
        return simplify(R, frame)

    return self


class Evaluate(RBaseType):
    """Do the combine arguments and the evaluate in the same operator.  Will just
    make the opetimizer aware of this operation the same way it is aware of ReflectStructure

    """

    def __init__(self, dyna_system, ret: Variable, term_var: Variable, extra_args: Tuple[Variable]=()):
        super().__init__()
        self.dyna_system = dyna_system
        self.ret = ret
        self.term_var = term_var
        self.extra_args = extra_args

    @property
    def vars(self):
        return (self.ret, self.term_var, *self.extra_args)

    def rename_vars(self, remap):
        return Evaluate(self.dyna_system, remap(self.ret), remap(self.term_var), tuple(remap(v) for v in self.extra_args))

    def _tuple_rep(self):
        return self.__class__.__name__, self.ret, self.term_var, self.extra_args


@simplify.define(Evaluate)
def simplify_evaluate(self, frame):
    if self.term_var.isBound(frame):
        # then we are going to replace this operation with the CallTerm operator
        t = self.term_var.getValue(frame)
        if not isinstance(t, Term):
            return Terminal(0)
        r = self.dyna_system.call_term(t.name, len(t.arguments)+len(self.extra_args))(*(constant(a) for a in t.arguments), *self.extra_args, ret=self.ret)
        return simplify(r, frame)

    return self

@optimizer.define(Evaluate)
def optimizer_evaluate(self, info):
    cc = info.conjunctive_constraints[self.term_var]
    for c in cc:
        if isinstance(c, BuildStructure) and c.result == self.term_var:
            # then we can determine the name and number of arguments from this
            name = c.name
            arity = len(c.arguments)
            vs = [VariableId(('evaluate_t', object())) for _ in range(arity)]
            c = self.dyna_system.call_term(name, arity+len(self.extra_args))(*vs, *self.extra_args, ret=self.ret)
            return Intersect((BuildStructure(name, self.term_var, vs), c))

    return self




class CallTerm(RBaseType):
    """
    This is a call to an external expression that has not yet been included.  If the modes match, then we could attempt
    to perform evaluation outside of the local context, otherwise we are just going to return the body of an expression
    """

    def __init__(self, var_map: Dict[Variable,Variable], dyna_system, term_ref):
        super().__init__()
        # this needs to have some positional arguments or something so that we
        # can use a tuple in tracking?

        self.var_map = var_map
        self.dyna_system = dyna_system  # this should become the local dynabase in the future
        self.term_ref = term_ref

        # for helping detect the case where backwards chaining recursion is ok
        self.parent_calls_blocker = []  # tuples of the variables that are
        # used by parent calls to this needs to identify when it is in a
        # backwards chaining recursive loop?  but if we are not just pushing
        # things as eagerly as possible, we might miss them...

        # self.replaced_with = None

    @property
    def vars(self):
        return tuple(self.var_map.values())

    def rename_vars(self, remap):
        r = CallTerm(dict((k, remap(v)) for k,v in self.var_map.items()), self.dyna_system, self.term_ref)
        r.parent_calls_blocker = [tuple(remap(v) for v in vv) for vv in self.parent_calls_blocker]
        return r

    def _tuple_rep(self):
        return self.__class__.__name__, self.term_ref, self.var_map

    def __eq__(self, other):
        return super().__eq__(other) and self.parent_calls_blocker == other.parent_calls_blocker

    def __hash__(self):
        return super().__hash__()

    def __lt__(self, other):
        if isinstance(other, CallTerm):
            if self.term_ref != other.term_ref:
                return self.term_ref < other.term_ref
            # then the term maps should be the same, so we are going to loop over the keys
            for k in sorted(self.var_map):
                if self.var_map[k] != other.var_map[k]:
                    return self.var_map[k] < other.var_map[k]
            return False
        return super().__lt__(other)


@simplify.define(CallTerm)
def simplify_call(self, frame):
    if not self.parent_calls_blocker:
        for c in reversed(frame.call_stack):
            if c.term_ref == self.term_ref:
                # then don't try to run this
                assert tuple(self.var_map.keys()) == tuple(c.var_map.keys())  # check that the ordres are the same, will have to remap otherwise
                r = CallTerm(self.var_map, self.dyna_system, self.term_ref)
                r.parent_calls_blocker += c.parent_calls_blocker
                r.parent_calls_blocker.append(tuple(c.var_map.values()))
                return r
                # self = r
                # break

    # sanitity check for now
    # in the case this fails, then it is likely that the python program would get a stack overflow exception without this block

    if len(self.parent_calls_blocker) >= self.dyna_system.stack_recursion_limit:
        err = f'Dyna backchaining stack depth has exceeded {self.dyna_system.stack_depth_limit} recurisve frames'
        suggested_prompt = suggest_api = None
        if isinstance(self.term_ref, tuple) and len(self.term_ref) == 2:
            name, arity = self.term_ref
            if isinstance(name, str) and isinstance(arity, int):
                err += '\nPossible fix is to memoize the intermeidate results and limit the stack depth, Eg:'
                suggested_prompt = f'memoize_unk {name}/{arity}'
                suggest_api = f'set the expression be memoized `api.make_call("{name}/{arity}").set_memoized("unk")`\n or increase the stack recursion limit with `api.stack_recursion_limit = 100`'
        raise DynaSolverErrorSuggestPrompt(err, suggested_prompt, suggest_api)

    # assert len(self.parent_calls_blocker) < 10

    # check if the arguments are unique, otherwise don't try and run this
    vs = [tuple(v.getValue(frame) for v in vv) for vv in self.parent_calls_blocker]
    a = tuple(v.getValue(frame) for v in self.var_map.values())

    if a in vs:
        # then don't try and run this
        return self

    # we might want to run simplify on this the first time?
    # this still doesn't handle the cases where we are going to be backwards chaining
    R = self.dyna_system.lookup_term(self.term_ref)
    R2 = R.rename_vars_unique(self.var_map.get)

    try:
        # we are going to eagerly build this structure, which is going to run
        # until we hit a backwards chaining cycle we might instead identify
        # things which are cycles when something is loaded into the program, and
        # then use that to when this sort of stuff should be applied?
        #
        # I suppose that we are going to try and make this thing work in the
        # case that we are not making guesses, which implies that there are
        # going to be memos
        frame.call_stack.append(self)
        R2 = simplify(R2, frame)
    finally:
        assert frame.call_stack[-1] is self
        del frame.call_stack[-1]

    return R2



inline_all_calls = Visitor()

@inline_all_calls.default
def inline_all_calls_default(self, inlined_calls, stack=()):
    return self.rewrite(lambda x: inline_all_calls(x, inlined_calls, stack))


@inline_all_calls.define(CallTerm)
def inline_all_calls_callterm(self, inlined_calls, stack=()):
    if self.term_ref in stack:
        # then we are going around a cycle, and this is not handled
        return self
    #raise RecursionError(self.term_ref)
    inlined_calls.add(self.term_ref)  # track that this was inlined at some point so that we can track the assumption
    R = self.dyna_system.lookup_term(self.term_ref)
    R2 = R.rename_vars_unique(self.var_map.get)
    return inline_all_calls(R2, inlined_calls, stack+(self.term_ref,))

@inline_all_calls.define(Evaluate)
def inline_all_calls_evaluate(self, inlined_calls, stack=()):
    raise RuntimeError('Evaluate can not determine what it will call')
