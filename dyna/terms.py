

from functools import reduce
import operator

from .interpreter import *
from .optimize import optimizer


class Term:
    # This should probably be renamed from "term" to "named tuple" or something
    # "term" is just overused in the system and there are other values that we
    # can represent in the system

    __slots__ = ('__name', '__arguments', '__hashcache')

    def __init__(self, name, arguments):
        self.__name = name
        assert all(not isinstance(a, Variable) for a in arguments)
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable
        self.__hashcache = hash(self.name) ^ reduce(operator.xor, map(hash, self.arguments), 0)

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments

    def __eq__(self, other):
        return self is other or \
            (isinstance(other, Term) and
             hash(self) == hash(other) and
             self.name == other.name and
             len(self.arguments) == len(other.arguments) and
             all(a == b for a,b in zip(self.arguments, other.arguments)))

    def __hash__(self):
        return self.__hashcache

    def __str__(self):
        return f'{self.__name}({", ".join(map(str, self.__arguments))})'

    def __repr__(self):
        return str(self)

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



class BuildStructure(RBaseType):
    """
    Build something like X=&foo(Y).
    """

    def __init__(self, name :str, result :Variable, arguments :List[Variable]):
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
        for var, val in zip(self.arguments, res.arguments):
            var.setValue(frame, val)
        return Terminal(1)
    elif all(v.isBound(frame) for v in self.arguments):
        # then the result must not be bound, so we are just going to construct it
        res = Term(self.name, [v.getValue(frame) for v in self.arguments])
        self.result.setValue(frame, res)
        return Terminal(1)

    return self



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
        self.result = result  # the resulting variable that we are trying to reflect
        self.name = name  # the variable that is going to take on the string value for the name
        self.num_args = num_args  # the number of arguments (length of the list), will let us rewrite in the case that not fully ground
        self.args_list = args_list  # the list of arguments that are found

    @property
    def vars(self):
        return (self.result, self.num_args, self.args_list)

    def rename_vars(self, remap):
        return ReflectStructure(
            remap(self.result),
            remap(self.name),
            remap(self.num_args),
            remap(self.args_list)
        )

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
    elif self.name.isBound(frame) and self.args_list.isBound(frame):
        # then we are going to be able to construct this object.  so we are
        # going to have to walk the list and convert it back into something that we want?
        name = self.name.getValue(frame)
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
        self.num_args.setValue(frame, len(res.arguments))
        self.result.setValue(frame, res)
        return Terminal(1)
    elif self.name.isBound(frame) and self.num_args.isBound(frame):
        assert not self.args_list.isBound(frame)
        name = self.name.getValue(frame)
        num_args = self.num_args.getValue(frame)

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

        R = Intersect(tuple(consts))
        return simplify(R, frame)

    return self

@optimizer.define(ReflectStructure)
def optimzier_reflectstructure(self, info):
    # this should check if there are other conjunctive constraints that contain
    # the type info, and then we can use those to rewrite this constraint to not exist
    raise NotImplementedError()


class Evaluate(RBaseType):
    """
    This should completement the reflect structure operator in that if we know the name and number of arguments then we can resolve the call
    without haivng to know the all of the arguments as ground.  To construct an `*X` operator then we can combine this with reflect structure
    so that both of these operators will get rewritten together
    """

    def __init__(self, dyna_system, ret :Variable, name :Variable, nargs :Variable, args_list :Variable):
        self.ret = ret
        self.name = name
        self.nargs = nargs
        self.args_list = args_list
        self.dyna_system = dyna_system

    @property
    def vars(self):
        return self.ret, self.name, self.nargs, self.args_list

    def rename_vars(self, remap):
        return Evaluate(self.dyna_system, remap(self.ret_var), remap(self.name_var), remap(self.nargs_var), remap(self.args_list))

@simplify.define(Evaluate)
def simplify_evaluate(self, frame):
    if self.args_list.isBound(frame) and not self.nargs.isBound(frame):
        # then we are going to compute the length
        args = self.args_list.getValue(frame)
        args = args.aslist()  # TODO: catch an errors here???
        self.nargs.setValue(frame, len(args))
    if self.name_var.isBound(frame) and self.nargs_var.isBound(frame):
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

        R = Intersect(tuple(consts))
        return simplify(R, frame)

    return self


class CallTerm(RBaseType):
    """
    This is a call to an external expression that has not yet been included.  If the modes match, then we could attempt
    to perform evaluation outside of the local context, otherwise we are just going to return the body of an expression
    """

    def __init__(self, var_map: Dict[Variable,Variable], dyna_system, term_ref):
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
                self = r
                break

    # sanitity check for now
    assert len(self.parent_calls_blocker) < 10

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
        raise RecursionError(self.term_ref)
    inlined_calls.add(self.term_ref)  # track that this was inlined at some point so that we can track the assumption
    R = self.dyna_system.lookup_term(self.term_ref)
    R2 = R.rename_vars_unique(self.var_map.get)
    return inline_all_calls(R2, inlined_calls, stack+(self.term_ref,))

@inline_all_calls.define(Evaluate)
def inline_all_calls_evaluate(self, inlined_calls, stack=()):
    raise RuntimeError('Evaluate can not determine what it will call')
