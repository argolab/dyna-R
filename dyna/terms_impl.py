
from .interpreter import Variable

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

    # def getName(self):
    #     # seems that using a java wrapper would not allow us to use @property stuff on the classes which are wrapped
    #     return self.__name

    @property
    def arity(self):
        return len(self.__arguments)

    def get_arity(self):
        return len(self.__arguments)

    def get_argument(self, idx):
        return self.__arguments[idx]

    @property
    def arguments(self):
        print('XXXX depricated Term.arguments, use Term.get_argument and Term.arity')
        #import ipdb; ipdb.set_trace()
        return self.__arguments

    def builtin_eq(self, other):
        # perform an equal operation using only builtin compares
        return self is other or \
            (isinstance(other, Term) and
             hash(self) == hash(other) and
             self.name == other.name and
             len(self.__arguments) == len(other.__arguments) and
             all(_builtin_eq(a,b) for a,b in zip(self.__arguments, other.__arguments)))

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

    def make_list(self):
        return self.fromlist(self.__arguments)

    def make_pylist(self):
        return self.__arguments

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


class TermConstructor:

    def __init__(self, name, arity):
        self.__name = name
        self.__arity = arity

    def __call__(self, arguments):
        assert len(arguments) == self.__arity
        return Term(self.__name, arguments)
