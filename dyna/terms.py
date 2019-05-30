
from .interpreter import *
from functools import reduce
import operator


class Term:

    __slots__ = ('__name', '__arguments')

    def __init__(self, name, arguments):
        self.__name = name
        assert all(not isinstance(a, Variable) for a in arguments)
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments

    def __eq__(self, other):
        return isinstance(other, Term) and (
            self.name == other.name and
            len(self.arguments) == len(other.arguments) and
            all(a == b for a,b in zip(self.arguments, other.arguments)))

    def __hash__(self):
        # this should be cached?
        return hash(self.name) ^ reduce(operator.xor, map(hash, self.arguments))

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
        res = Term(self.name, (v.getValue(frame) for v in self.arguments))
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



    # TODO: this needs to be able to perform the rewrite in the case that it
    # only knows the name and the list length atm, this will require something
    # that is higher level to be able to determien what the length of the list
    # is.  If there was some additional argument, then maybe that could be used
    # to perform the local rewrite.
    return self



# class ExtendStructure(RBaseType):
#     """
#     Extend a term by adding additional variables to the output
#     something like: X=&foo(Y), Z=&foo(Y, W) $extend(X, Z, W)

#     This can be used as sugar for something like *X(A), where we are calling the method referenced by X with the additional parameters A

#     This could just use the reflect structure above, though
#     """

#     def __init__(self, inp: Variable, out: Variable, addition :List[Variable]):
#         pass


class Evaluate(RBaseType):
    """
    *X, evaluation construct where we lookup the name and the number of arguments.
    This is only takes a single variable (there is no return variable) and will rewrite as the R-expr that defines that term
    In the case that return arguments are required, then that should be done with ExtendStructure
    """

    def __init__(self, var):
        pass



class CallTerm(RBaseType):
    """

    """

    def __init__(self, name :str, arguments: List[Variable], dynabase, term_name):
        self.name = name
        self.arguments = arguments
        self.dynabase = dynabase
        self.term_name = term_name

        self.replaced_with = None


@simplify.define(CallTerm)
def simplify_call(self, frame):
    # we want to keep around the calls, so that we can continue to perform replacement operations on stuff.

    return self
