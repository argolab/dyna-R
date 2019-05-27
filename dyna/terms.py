
from .interpreter import *


class Term:

    __slots__ = ('__name', '__arguments')

    def __init__(self, name, arguments):
        self.__name = name
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments



class BuildStructure(RBaseType):
    """
    Build something like X=&foo(Y).
    """

    def __init__(self, name :str, result :Variable, arguments :List[Variable]):
        pass

class ReflectStructure(RBaseType):
    """
    For reflecting the type of the quoted object with the name as a string and the body as a list of cons cells

    This should rewrite as BuildStructure as early as possible.
    So if name is a known constant and the body is a fully formed list that we can walk abstractly.
    """

    def __init__(self, result: Variable, name :Variable, body :Variable):
        pass


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
