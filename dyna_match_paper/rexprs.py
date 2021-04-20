# a simple as possible implementation of R-exprs rewriting engine
# the term

import sys
import os

from collections import defaultdict
from functools import cache
from textwrap import indent

from contextlib import contextmanager


####################################################################################################
# utility functions

@cache
def parse_sexp(s):
    # super basic s-expression parser
    sexpr = s.replace('(', ' ( ').replace(')', ' ) ').split()  # lex the expression
    stack = [[]]
    for symbol in sexpr:
        if symbol == '(':
            stack.append([])  # push stack
        elif symbol == ')':
            val = tuple(stack.pop())
            stack[-1].append(val)
        else:
            stack[-1].append(symbol)
    if len(stack) != 1:
        raise RuntimeError(f'S Expression is ill-formed: {s}')
    return tuple(stack[0])


####################################################################################################
# for helping with tracking the rewrites (to generate "figures" in the paper)

logging_rewrites = False
active_rewrite = []

@contextmanager
def named_rewrite(name):
    """
    This can be wrapped around the matching expression such that we can log which name should be applied to an expression
    These can then be used to generate output which appears
    """
    global active_rewrite
    active_rewrite.append(name)
    yield None
    assert active_rewrite[-1] == name
    active_rewrite.pop()

def track_rexpr_constructed(func):
    if not logging_rewrites:
        return func
    def nf(*args):
        r = func(*args)
        if isinstance(r, Term):
            # maybe this should only include some of the arguments
            r._debug_constructed_from.append(args)
        return r


if logging_rewrites:
    logging_rewrite_output = open(os.environ.get('REWITE_LOG', 'rewrite.log'), 'w+')
    import uuid



####################################################################################################
# base definitions of Term structure.
# Every term has a name and a variable number of arguments

class Term:

    __slots__ = (
        ('__name', '__arguments', '__hashcache') +
        (('_debug_active_rewrite', '_debug_constructed_from', '_debug_unique_id')
         if logging_rewrites else ())
    )

    def __init__(self, name, arguments):
        #assert all(not isinstance(a, Variable) for a in arguments) and isinstance(name, str)  # there is not a special symbol for variables in this version
        assert isinstance(name, str)
        self.__name = name
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable
        self.__hashcache = hash(self.name) ^ hash(self.__arguments)

        if logging_rewrites:
            self._debug_active_rewrite = active_rewrite.copy()
            self._debug_constructed_from = []
            self._debug_unique_id = f'gened_term:{self.name}_{self.arity}_{uuid.uuid4().hex}'  # make a unique id for this

    @property
    def arity(self):
        return len(self.__arguments)

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments

    def get_argument(self, idx):
        return self.__arguments[idx]

    def __iter__(self):
        yield self.__name
        yield from self.__arguments

    def __hash__(self):
        return self.__hashcache

    def __eq__(self, other):
        return self is other or \
            (isinstance(other, Term) and \
             self.__hashcache == other.__hashcache and \
             self.__name == other.__name and \
             self.__arguments == other.__arguments)

    def __str__(self):
        return self.name + '(' + ', '.join(map(str, self.arguments)) + ')'

    def __repr__(self): return str(self)

    def indented_str(self, indent=0):
        if self.name == '$VARIABLE' and self.arity == 1:
            return ' '*indent + f'$VARIABLE({self.arguments[0]})'
        ret = [' '*indent + self.name]
        if self.arity > 0:
            ret.append('(\n')
        for a in self.arguments:
            if hasattr(a, 'indented_str'):
                ret.append(a.indented_str(indent+1))
                ret.append(',\n')
            else:
                ret.append(' '*(indent + 1))
                ret.append(str(a))
                ret.append(',\n')
        if self.arity > 0:
            ret.append(' '*indent + ')')
        return ''.join(ret)

    def stylized_rexpr(self):
        # return a string for this which is supposed to be close to representtaion used in the paper as possible

        def nested(r):
            # print out a nested R-expr, if the expression is big, then it will indent, otherwise it will try and make it inline
            if isinstance(r, Term):
                s = r.stylized_rexpr()
            else:
                s = str(r)
            s = s.strip()
            if '\n' in s or len(s) > 50:
                s = '\n' + indent(s, '  ') + '\n'
            return s

        if self.name == '$VARIABLE' and self.arity == 1:
            v = self.get_argument(0)
            if isinstance(v, int):
                # becuase ints are used for the arguments, they can be hard to
                # see in the argument place, so need something more in the
                # representation here
                v = f'$ARG_{v}'
            return str(v).capitalize()
        elif self.name == '=' and self.arity == 2:
            return f'({nested(self.get_argument(0))}={nested(self.get_argument(1))})'
        elif self.name == 'aggregator' and self.arity == 4:
            return f'({nested(self.get_argument(1))}={nested(self.get_argument(0))}({nested(self.get_argument(2))}, {nested(self.get_argument(3))}))'
        elif self.name == 'structure':
            return f'({nested(self.get_argument(1))}={nested(self.get_argument(0))}(' + ', '.join(map(nested, self.arguments[2:])) + '))'
        elif self.name in ('+', '*'):
            return self.name.join(map(nested,self.arguments))
        elif self.name == 'proj' and self.arity == 2:
            # then this is a projection expression, if the nested expression is
            # also a projection, then we we are not going to nest the expression
            # so we will get something like proj(A, proj(B,  stuff ))
            var, body = self.arguments
            var = nested(var)
            if isinstance(body, Term) and body.name == 'proj' and body.arity == 2:
                bodyv = body.stylized_rexpr().strip()
                return f'proj({var}, {bodyv})'
            else:
                return f'proj({var}, {nested(body)})'
        else:
            # this covers all base cases and expressions like if which are just represented via their name
            return f'{self.name}(' + ', '.join(map(nested, self.arguments)) + ')'



def Variable(name):
    if isinstance(name, Term) and name.name == '$VARIABLE':
        return name
    return Term('$VARIABLE', (name,))

def isVariable(x):
    return isinstance(x, Term) and x.name == '$VARIABLE' and x.arity == 1


class _multiplicityTerm(Term):
    def __init__(self, val):
        assert (isinstance(val, int) and val >= 0) or val == float('inf')
        super().__init__('$MUL', (val,))
    def __add__(self, other):
        if not isinstance(other, (int, float)):
            assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
            other = other.arguments[0]
        return type(self)(self.arguments[0] + other)
    def __mul__(self, other):
        if not isinstance(other, (int, float)):
            assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
            other = other.arguments[0]
        a = self.arguments[0]
        # if either value is inf, then having 0*inf == nan, but we would like it to be 0
        if a == 0 or other == 0:
            return type(self)(0)
        return type(self)(a * other)
    __rmul__ = __mul__  # the direction of this does not matter, but it might be called with an int on the lhs
    __radd__ = __add__
    def __int__(self): return self.arguments[0]
    def __eq__(self, other):
        if isinstance(other, (int, float)):
            return other == self.arguments[0]
        return super().__eq__(other)
    def __hash__(self): return super().__hash__()

def multiplicity(val):
    if isinstance(val, int):
        return _multiplicityTerm(val)
    if isinstance(val, _multiplicityTerm):
        return val
    if val == float('inf'):
        return _multiplictyTerm(val)
    assert False  # wtf

def isMultiplicity(val):
    return isinstance(val, _multiplicityTerm)

####################################################################################################
# The core of the rewriting engine and pattern matching
# These classes are designed to be "mostly" general in that they do not specialize to specific rewrites
# These do implement the R-expr rewrites which


class UnificationFailure(Exception):
    pass

class RewriteContext(set):
    """This is the context in which a given rewrite is being performed.  This can
    be though of as an (incomplete) set of conjunctive R-exprs to the current
    R-expr.  There are methods which are designed to assist with looking up
    expressions more efficiently and retrieving the assignment of a variable
    """

    def __init__(self, parent=None, *, set_vals=None):
        super().__init__()
        self._parent = parent
        self._assign_index = {}
        self._unifies_index = defaultdict(set)
        self._kind_index = defaultdict(set)
        self._argument_index = defaultdict(set)

        if set_vals is not None:
            for s in set_vals:
                self.add_rexpr(s)

        # if set_vals is not None:
        #     super().__init__(set_vals)
        #     self._build_indexes()
        # else:
        #     super().__init__()
        # # self._multiplicity = 1  # a tracker on the multiplicity of the
        # #                         # expression so things get multiplied together

    def _index_unify_rexpr(self, r):
        a,b = r.arguments
        if isVariable(a) and isVariable(b):
            # then this is a unification between two variables with neither ground
            # if either of these are set, then the other one should also become set
            av, bv = self.get_value(a), self.get_value(b)
            if av is None and bv is None:
                # then this is just a unification between these two variables
                self._unifies_index[a].add(b)
                self._unifies_index[b].add(a)
            elif av is None:
                # then assign a the value of the variable b
                self._set_variable(a, bv)
            elif bv is None:
                self._set_variable(b, av)
            elif av != bv:
                # both are assigned, but they are not values that unify together
                raise UnificationFailure()
        else:
            if isVariable(b) and not isVariable(a):
                # swap a and b so that a is always the variable
                a,b = b,a
            av = self.get_value(a)
            if av is not None:
                if av != b:
                    # the value of b does not match
                    raise UnificationFailure()
            else:
                # save the result of this assignment in the index
                self._set_variable(a, b)

    # def _build_indexes(self, adding):
    #     for r in adding:
    #         self.add_rexpr(r)
    #         # if r.name == '=' and r.arity == 2:
    #         #     # if there are two values for the
    #         #     self._index_unify_rexpr(r)

    def _set_variable(self, var, val):
        assert isVariable(var)
        cv = self.get_value(var)
        if cv is not None:
            if cv != val:
                raise UnificationFailure()
        else:
            assert not contains_any_variable(val)
            #print(f'Variable assigned {var}={val}')
            self._assign_index[var] = val
            # set the value of all variables that this variable is unified with
            # so this is eager propagating in the context for unification of constants
            # which is maybe a slight difference from what is written in the paper currently?
            for unified in self._get_unified(var):
                self._set_variable(unified, val)

    def _get_unified(self, var):
        if self._parent is not None:
            yield from self._parent._get_unified(var)
        if var in self._unifies_index:
            yield from self._unifies_index[var]


    def add_rexpr(self, r):
        assert isinstance(r, Term)
        if self.__contains__(r):
            # this is already tracked, sowe are not going to add it again to the expression
            return

        if isMultiplicity(r):
            # we ignore tracking of multiplicies here as this is just a _set_
            # these are returned else where such that it will find
            return

        if r.name in ('+', '*') or (r.name, r.arity) in (('proj', 2), ('aggregate', 4), ('if', 3)):
            # ignore expressions which are the disjunctions/conjunctions themselves.
            # These are nested expressions
            return

        # so that we can find things like lessthan(A,B) by just looking under lessthan/2
        self._kind_index[(r.name, r.arity)].add(r)
        super().add(r)  # this is just tracking the standard r-expr term

        for a in r.arguments:
            # track that this argument appeared in this expression
            self._argument_index[a].add(r)

        if r.name == '=' and r.arity == 2:
            self._index_unify_rexpr(r)

    # override the set method to also add rexprs
    add = add_rexpr

    def __contains__(self, v):
        if super().__contains__(v):
            return True
        if self._parent is not None:
            return self._parent.__contains__(v)
        return False

    # for dealing with recursive operations, this will introduce a new copy and
    # then use set intersection and subtraction to extract the relevant parts of
    # the expression
    def copy(self):
        return RewriteContext(self)

    def __and__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        # this is the things which are shared between both of these expressions
        # if there is something which
        if self is o:
            # this would need to be a copy, so it would want to copy itself
            return self

        # this is going to rebuild the index which is "slow" but should still be ok with this working
        res = RewriteContext(parent=self._parent, set_vals=super().__and__(o))
        return res

    def __sub__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        # if this is subtracting one thing from another, then the parent should not be included
        if self is o:
            # tihs is going to have to rewrite an empty environment as these are equivalent or something?
            return RewriteContext()

        if bool(o) is False:
            # then the other thing is empty, so we can just return ourselves without changes
            return self

        import ipdb; ipdb.set_trace()
        assert False

    # don't use the inplace expressions for now, though these might make it more efficient in the future?
    def __iand__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent

        if self is o:
            # this is itself, there is no modification that is required
            return self
        return NotImplemented

    def __isub__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent

        if self is o:
            # this is going to have to return an empty value, which means that
            return NotImplemented

        return NotImplemented

    def get_value(self, variable):
        if isVariable(variable):
            r = self._assign_index.get(variable)
            if r is None and self._parent is not None:
                return self._parent.get_value(variable)
            return r
        else:
            assert not contains_any_variable(variable)
            # this is not a variable, so just return the value back.  if there is nothing here,
            return variable

    def get_all_kinds(self, name, arity):
        r = self._kind_index.get((name, arity), set())
        if self._parent is not None:
            r |= self.get_all_kinds(name, arity)
        return r

    def iter_local(self):
        return super().__iter__()

    def iter_all(self):
        if self._parent is not None:
            yield from self._parent.iter_all()
        yield from self.iter_local()

    def local_to_rexpr(self):
        # this is going to have to take all of the local conjunctive information and turn it into a R-expr which can

        # if there are assignments, then those should be placed first

        #res = tuple(self.iter_local())

        res = []
        for var, val in self._assign_index.items():
            res.append(Term('=', (var, val)))  # this is just the assignments to ground variables, all of the other expressions should still be in the R-expr

        r = make_conjunction(*res)  # this is just a conjunction of the constraints which are present

        #import ipdb; ipdb.set_trace()
        return r

    def get_associated_with_var(self, var):
        assert isVariable(var)
        if self._parent is not None:
            yield from self._parent.get_associated_with_var(var)
        if var in self._argument_index:
            yield from self._argument_index[var]

    def pop_variable(self, var):
        assert isVariable(var)
        # this will need to remove anything which contains the variable
        # this means that these expressions are no longer

        raise NotImplementedError()

class RewriteCollection:
    """A class for tracking rewrite operators.  Operators are segmented such that
    they can be looked up quickly using the name of an R-expr or in the case
    that more advanced matching is required

    """
    def __init__(self):
        self.name_match = defaultdict(list)
        self.name_arity_match = defaultdict(list)
        self.full_match = defaultdict(list)

        # this would be some one-to-one matching of an expression
        # these are not "functions" which correspond with builtins, but rather R-exprs
        # that are used to replace the given expression
        self.user_defined_rewrites = {}

    def _register_function(self, pattern, func):
        # determine which of the patterns are required for a given expression
        patterns = parse_sexp(pattern)
        for pattern in patterns:
            any_arity = False
            complex_pattern = False
            arity = None
            name, *args = pattern
            assert isinstance(name, str)
            for i, v in enumerate(args):
                if isinstance(v, tuple): complex_pattern = True # there is more here to match
                elif v == 'any': any_arity = True
                elif v == 'args': arity = int(args[i+1])
            if complex_pattern:
                self.full_match[pattern].append(func)
            elif any_arity:
                self.name_match[name].append(func)
            else:
                if arity is None:
                    arity = len(args)
                self.name_arity_match[(name, arity)].append(func)

    def register_function(self, pattern):
        def f(func):
            self._register_function(pattern, func)
            return func
        return f

    def do_user_defined_rewrite(self, rewrite_engine, rexpr):
        # this should take the name arity of a given expression and then subsuite in new names for the variables
        n = (rexpr.name, rexpr.arity)
        rxp = self.user_defined_rewrites[n]
        # this will have to subsuite in the variable names and create new variable names for the expression
        var_map = {}
        for i, new_name in enumerate(rexpr.arguments):
            var_map[Variable(i)] = new_name

        # this will have to replace all of the variables or create new variable names for all of the expression
        rxp = uniquify_variables(rxp, var_map)

        # we DO NOT immediatly apply rewrites to the returned expression as that could cuase recursive programs to run forever

        # TODO: the depth lmiting is not included in this version currently.  This will have to walk through the R-expr
        # and identify user calls and add a call depth as some extra hidden meta data on the parameter or something

        return rxp

    def define_user_rewrite(self, name, arity, rexpr):
        var_map = {}
        for i in range(arity):  # this will ensure that these variables keep
                                # their old name rather than getting replaced
            v = Variable(i)
            var_map[v] = v
        rexpr = uniquify_variables(rexpr, var_map)
        self.user_defined_rewrites[(name, arity)] = rexpr

    def get_matching_rewrites(self, rexpr, context=None):
        if rexpr.name in self.name_match:
            yield from self.name_match[rexpr.name]
        n = (rexpr.name, rexpr.arity)
        if n in self.name_arity_match:
            yield from self.name_arity_match[n]
        # these full matches require that there is more contextual information for this
        # which means that this is going to be looking.
        # we might want to avoid doing the full matches most of the time, so this will need to be controllable
        # by some expression
        for pattern, funcs in self.full_match.items():
            if match(context, rexpr, pattern):
                yield from funcs

        # in the case that the name/arity matches the user defined rewrite, then the generic user
        # rewrite handler will handle it
        if n in self.user_defined_rewrites:
            yield self.do_user_defined_rewrite


class RewriteEngine:
    """
    Recursively applies itself to the R-expr until it is rewritten
    """

    def __init__(self, *, rewrites: RewriteCollection=None, kind='simple', context=None):
        self.__rewrites = rewrites or globals()['rewrites']
        assert kind in ('simple', 'full')
        self.__kind = kind

        # context can be modified as this moves through the different rewrites
        self.context = context or RewriteContext()

    # these are read only, so make access through a property
    @property
    def kind(self): return self.__kind

    @property
    def rewrites(self): return self.__rewrites

    @track_rexpr_constructed
    def apply(self, rexpr):
        try:
            for func in self.rewrites.get_matching_rewrites(rexpr, self):
                res = func(self, rexpr)
                assert res is not None  # error as this means the implementation is incomplete
                if res != rexpr:
                    self.context.add_rexpr(res)  # this is going to add the new R-expr to the context
                    return res  # stop trying to match the expression and accept this rewrite
            # no rewrite matched, so this is just going to return the R-expr unmodified
            self.context.add_rexpr(rexpr)  # still add to the environment to track
            return rexpr
        except UnificationFailure:
            return multiplicity(0)

    # def __call__(self, rexpr):
    #     return self.apply(rexpr)

    def rewrite_once(self, rexpr):
        rexpr = self.apply(rexpr)
        return make_conjunction(self.context.local_to_rexpr(), rexpr)

    def rewrte_fully(self, rexpr):
        while True:
            # this contains the context values inside of itself
            old = rexpr
            rexpr = self.apply(rexpr)
            if old == rexpr: break
        return make_conjunction(self.context.local_to_rexpr(), rexpr)

    def get_value(self, rexpr):
        # I think this method should get removed
        return self.context.get_value(rexpr)

# def fully_rewrite(rewrite_engine :RewriteEngine, rexpr):
#     while True:
#         old_rexpr = rexpr
#         rexpr = rewrite_engine(rexpr)
#         if old_rexpr == rexpr:  # meaning that there were no rewrites applied to the expression
#             break

def match(self :RewriteContext, rexpr, pattern, *pattern_args):
    # the pattern should be something which returns the variable
    # I suppose that we could use a for loop and then this would return an iterator in the case that the pattern matches

    # match might be outside
    if isinstance(pattern, str):
        expr = parse_sexp(pattern)
    else:
        assert isinstance(pattern, tuple)
        expr = pattern

    returning_match = []

    def rec(rexpr, pattern):
        if isinstance(pattern, str):
            name = pattern
        else:
            name = pattern[0]
        if name == 'OR':
            for child in pattern[1:]:
                res = rec(rexpr, child)
                if res is not None:
                    return res
            return None  # failed to match
        elif name == 'AND':
            res = []
            for child in pattern[1:]:
                res = rec(rexpr, child)
                if res is None:
                    return None  # failed to match one of the expressions
            return res  # match was successful, return the last thing
        elif name == 'NOT':
            res = rec(rexpr, pattern[1])
            if res is None:
                return []  # the match was unsuccessful, so this means we are negated in what is matched
            else:
                # there was some mat
                return None
        elif name == 'GET-VALUE':
            # this is going to need to lookup the value of some variable from the envrionment
            res = rec(rexpr, pattern[1])
            if res is None or len(res) != 1:
                return None  # match failed or returned something unexpected
            if isVariable(res[0]):
                v = self.get_value(res[0])
                if v is None:
                    return None
                res = [v]
            return res
        # TODO: maybe var, ground rexpr should be VAR GROUND REXPR so that it is clear they are meta
        elif name == 'var':
            if isVariable(rexpr):
                return [rexpr]  # this was a successful match, so return it in an array
            return None  # failed match
        elif name == 'ground':
            if isVariable(rexpr):
                if self is not None:
                    val = self.get_value(rexpr)
                    if val is not None:
                        return [val]
                else:
                     return None  # not ground, so fail match
            elif contains_any_variable(rexpr):
                # then this would require looking up the variables to match a given expression
                # if there is something that does not match, then that means that this would

                # question: if the variables have known values, should those get unified in when returning, or should this just error out
                # and let those get unifie din elsewhere first.

                assert False
            else:
                # this must be a ground value, so we can just return this
                return [rexpr]
        elif name == 'rexpr':
            return [rexpr]  # always matches
        elif name == 'param':
            idx = int(pattern[1])
            if rexpr == pattern_args[idx]:
                return []  # successful match returns nothing
            else:
                return None  # match failed

        elif name == 'match_param':
            idx = int(pattern[1])
            if rexpr == returning_match[idx]:
                return []  # successful matched something that was matched before
            else:
                return None
        elif name == 'read-param':
            idx = int(pattern[1])
            return [pattern_args[idx]]

        elif name == 'mul':
            try:
                val = int(pattern[1])
                if isMultiplicity(rexpr) and rexpr.get_argument(0) == val:
                    return []
                else:
                    return None
            except ValueError:
                if isMultiplicity(rexpr):
                    return []
                else:
                    return None

        # we are going to match against the name of the R-expr itself
        if rexpr.name != name:
            # the match has failed on the name alone
            return None
        if isinstance(pattern, str):
            if rexpr.arity == 0:
                return []  # match a 0-arity term
            else:
                return None
        # there might be variable length matching here, so we are not going to check that the length matches

        ret = []
        term_idx = 0
        match_idx = 1
        while match_idx < len(pattern):
            # if term_idx >= rexpr.arity:
            #     return None  # match failed
            n = pattern[match_idx]
            if n == 'any':
                # then this is going to match from here to the end of the expression
                ret.append(rexpr.arguments[term_idx:])
                term_idx = rexpr.arity
                break
            elif n == 'args':
                num = int(pattern[match_idx + 1])
                match_idx += 1
                if rexpr.arity == num:
                    return []  # successful match
                else:
                    return None  # unsuccessful
            else:
                res = rec(rexpr.get_argument(term_idx), n)
                if res is None:
                    return None  # failed recursive match
                ret += res
            term_idx += 1
            match_idx += 1
        if term_idx != rexpr.arity:
            # then there are more terms which are not matched here, so this is a match failure
            return None
        return ret

    #ret = []
    for e in expr:
        res = rec(rexpr, e)
        if res is None:
            return []  # meaning that the match has failed
        returning_match.extend(res)
    assert res is not None
    # this is to return an iterable over the values.  The returned iterable should always be length 1.
    # this match expression is either going to be used as `for val in match(....)` or `for a,b,c in match(...)`
    # which means that it needs to return the value as something that can be unpacked
    if len(returning_match) == 0:
        return [None] # this means the matches was successful, but we do not
                      # want to return an empty array as that means unsuccessful
    elif len(returning_match) == 1:
        # just return the array in this case as it will have the arguments
        # this should still be used as `for value in match(....)`
        return [returning_match[0]]
    else:
        # this has to return an iterable over the values, the
        return [returning_match]



rewrites = RewriteCollection()
register_rewrite = rewrites.register_function


####################################################################################################
# Rewrites

@register_rewrite('(* any)')
def multipliy_base(self, rexpr):
    ret = []
    mul = 1
    for r in rexpr.arguments:
        z = self.apply(r)
        if isMultiplicity(z):  # this is going to want to match the multiplicity for some value
            mul *= z
            if mul == 0: return mul  # this has hit the shortcut of reducing to nothing
        ret.append(z)

    if mul != 1:
        ret.insert(0, multiplicity(mul))

    return make_conjunction(*ret)


def make_conjunction(*args):
    # helper function which flattened nested * expressions
    ret = []
    mul = 1
    def add(x):
        nonlocal mul, ret
        if isMultiplicity(x):
            mul *= x
        elif x.name == '*':
            for a in x.arguments:
                add(a)
        else:
            assert isinstance(x, Term)
            ret.append(x)
    for a in args:
        add(a)
    if mul != 1:
        ret.insert(0, mul)
    if len(ret) == 0:
        return multiplicity(1)
    if len(ret) == 1:
        return ret[0]
    return Term('*', ret)


@register_rewrite('(+ any)')
def add_base(self, rexpr):
    if rexpr.arity == 1:
        # then there is only 1 disjunction, so we should just remove the disjunction expression
        return self.apply(rexpr)

    ret = []
    env_outer= self.context
    try:
        for r in rexpr.arguments:
            # this will need to merge the common environments together
            # which means that this will need
            self.context = env_outer.copy()
            try:
                r = self.apply(r)
                if not (isMultiplicity(r) and r == 0):  # ignore branches hwich are eleminated with 0 mult
                    ret.append((r, self.context))
            except UnificationFailure:
                assert False  # this should not get thrown (I think)
                # just ignore these branches
                pass

        if not ret:  # meaning that all ofthe branches are failed
            return multiplicity(0)

        # identify the common elements which are tracked in the environments by interesting the sets
        res_env = ret[0][1]
        for _, env in ret:
            res_env &= env

        for i in range(len(ret)):
            # now that the resulting environment has been identified, this can subtract
            # off that env and construct the resulting term for each sub expression
            re = ret[i][1] - res_env
            ret[i] = make_conjunction(ret[i][0], re.local_to_rexpr())

        env_outer = res_env
        self.context = env_outer
        return make_disjunction(*ret)
    finally:
        # this will need to update the env_prev with whatever is the new content which
        self.context = env_outer
    # this will need to determine what is the common sets of these elements.  From there it will

def make_disjunction(*args):
    ret = []
    mul = 0
    def add(x):
        nonlocal mul, ret
        if isMultiplicity(x):
            mul += x
        elif x.name == '+':
            for a in x.arguments:
                add(a)
        else:
            assert isinstance(x, Term)
            ret.append(x)
    for a in args:
        add(a)
    if mul != 0:
        ret.insert(0, mul)
    if len(ret) == 0:
        return multiplicity(0)
    if len(ret) == 1:
        return ret[0]
    return Term('+', ret)


@register_rewrite('(= args 2)')
def unify(self, rexpr):
    for a,b in match(self, rexpr, '(= ground ground)'):
        return multiplicity(1 if a == b else 0)
    for va, vb in match(self, rexpr, '(= var var)'):
        if va == vb:
            # this is trivally true, so just remove
            return multiplicity(1)

    for a,vb in match(self, rexpr, '(AND (NOT (= var rexpr)) (= rexpr var))'):
        # flip the direction
        rexpr = Term('=', (vb, a))

    # rexpr here matches any term.  So if nither of these have a variable, then it will match this expression
    for va, vb in match(self, rexpr, '(AND (NOT (= var rexpr)) (NOT (= rexpr var)) (= rexpr rexpr))'):
        # then either this is a structured term, and we need to expand this out, or there  is some variable.
        # in this case, we do not want to track it as an assignment
        assert isinstance(va, Term) and isinstance(vb, Term)

        assert False

        if va.name != vb.name or va.arity != vb.arity:
            # the arity on these expressions does not match
            return multiplicity(0)

        # this is something like (f(x,y,z)=f(a,b,c)) so we want to expanded and construct a new term

        ret = [Term('=', (aa,bb)) in zip(va.arguments, vb.arguments)]
        rr = make_conjunction(*ret)

        return self.apply(rr)

    # this is going to go into the environment, and then later pulled back out of the environment
    # so we don't want this to remain as it would end up duplicated
    self.context.add_rexpr(rexpr)
    return multiplicity(1)

@register_rewrite('(structure ground any)')
def unify_structure(self, rexpr):
    # there are a variable number of arguments
    for name, res_variable, args in match(self, rexpr, '(structure ground ground any)'):
        # this needs to unpack the variables in the expression
        if res_variable.name != name or res_variable.arity != len(args):
            # this has failed to match the given expression
            return multiplicity(0)
        ret = tuple(Term('=', (var, val)) for val, var in zip(res_variable.arguments, args))
        if len(ret) > 1:
            return Term('*', ret)
        else:
            return ret[0]
    for name, res_variable, args in match(self, rexpr, '(structure ground var any)'):
        # if all of the variables are ground, then we can construct the resulting term
        all_ground = True
        for var in args:
            if not match(self, var, 'ground'):
                all_ground = False
                break
        if all_ground:
            # then we can construct the resulting term for this expression
            values = []
            for var in args:
                for val in match(self, var, 'ground'):
                    values.append(val)


def make_project(*args):
    *var_names, rexpr = args
    for var in reversed(var_names):
        rexpr = Term('proj', (Variable(var), rexpr))
    return rexpr

@register_rewrite('(proj var rexpr)')
def proj(self, rexpr):
    for v, r in match(self, rexpr, '(proj var rexpr)'):
        outer_context = self.context
        try:
            self.context = outer_context.copy()
            rr = self.apply(r)  # this is going to apply rewrites to the inner body
            vv = self.context.get_value(v)

            if vv is not None:
                # then we are going to go through and do a replace and then just return the body
                rr2 = replace_term(rr, {v: vv})  # replace the variable with its value
                return rr2

            for _ in match(self, rr, '(mul 0)'):
                # if the body is zero, then this is also zero
                return multiplicity(0)

            #import ipdb; ipdb.set_trace()

            # remove disjunctive and conjunctive expressions out
            for ags in match(self, rr, '(+ any)'):
                ret = []
                for a in ags:
                    if vv is not None:
                        a = make_conjunction(Term('=', (v, vv)), a)
                    a = Term('proj', (v, a))
                    ret.append(a)
                return make_disjunction(*ret)

            for ags in match(self, rr, '(* any)'):
                not_depends = []
                depends = []
                if vv is not None:
                    depends.append(Term('=', (v, vv)))
                # anything which does not mention the variable v can be lifted out of the project statement
                for a in ags:
                    if contains_variable(a, v):
                        depends.append(a)
                    else:
                        not_depends.append(a)
                if not_depends:
                    return make_conjunction(*not_depends, Term('proj', (v, make_conjunction(*depends))))


            for _ in match(self, rr, '(= (param 0) ground)', v):
                # this is proj(X, (X=5)) -> 1
                return multiplicity(1)

            for _, _, nested_rexpr in match(self, rr, '(aggregator ground (param 0) var rexpr)', v):
                # this is proj(X, (X=sum(Y, ...)))
                if not contains_variable(nested_rexpr, v):
                    return multiplicity(1)


            return Term('proj', (v, rr))
        finally:
            self.context = outer_context



@register_rewrite('(aggregator ground var var rexpr)')
def aggregator(self, rexpr):

    # as described in the paper, the only way in which aggregators run is when there is a single element
    # though in practice we would like to handle multiple values directly.  Given that we are trying to be as close as possible to the paper
    # we are just going to always cause an aggregator R-expr to split before teh result is returned
    # operator = {
    #     'sum': sum,
    #     'prod': lambda a,b: a*b,
    #     'min': min,
    #     'max': max,
    # }

    identity = {
        'sum': 0,
        'prod': 1,
        'min': float('inf'),
        'max': float('-inf')
    }
    split_op = {
        'sum': 'plus',
        'prod': 'times',
        'min': 'min',
        'max': 'max'
    }

    for op, resulting, incoming, rxp in match(self, rexpr, '(aggregator ground var var rexpr)'):
        # this will need to match against the body of the expression
        outer_context = self.context
        try:
            self.context = outer_context.copy()

            #import ipdb; ipdb.set_trace()

            rxp = self.apply(rxp)  # this should attempt to simplify the expression
            for _ in match(self, rxp, '(mul 0)'):
                return Term('=', (resulting, identity[op]))
            for res in match(self, rxp, '(= (param 0) ground)', incoming):
                return Term('=', (resulting, res))
            if (inc_val := self.context.get_value(incoming)) is not None and isMultiplicity(rxp):
                # if the body is a multiplicity, then we should just return the resulting value
                assert rxp == 1  # TODO handle other multiplicies, will require knowing the sum_many operation
                return Term('=', (resulting, inc_val))
            for ags in match(self, rxp, '(+ any)'):
                # this is a disjunction between many different variables
                # this is going to have to construct many projects and new variables
                intermediate_vars = [generate_var() for _ in range(len(ags))]
                nested_exprs = []
                additional_exprs = []
                for nested_r, nv in zip(ags, intermediate_vars):
                    nested_exprs.append(Term('aggregator', (op, nv, incoming, nested_r)))
                while len(intermediate_vars) > 2:
                    # this is going to combine two of the variables together and generate a new variable to be the result
                    new_var = generate_var()
                    *intermediate_vars, v1, v2 = [new_var] + intermediate_vars
                    additional_exprs.append(Term(split_op[op], (v1, v2, new_var)))  # this is going to be like plus or times in the case

                if len(intermediate_vars) == 2:
                    additional_exprs.append(Term(split_op[op], (*intermediate_vars, resulting)))
                elif len(intermediate_vars) == 1:
                    additional_exprs.append(Term('=', (resulting, intermediate_vars[0])))
                else:
                    assert False  # shuld never happen

                # now this needs to construc the expression with all of the variables combined together
                nested_r = Term('*', nested_exprs + additional_exprs)
                for nv in intermediate_vars:
                    nested_r = Term('proj', (nv, nested_r))
                return nested_r
        finally:
            self.context = outer_context

        # there are no rewrites which can be applied here
        return rexpr

@register_rewrite('(if args 3)')
def if_rr(self, rexpr):
    for cond, true_r, false_r in match(self, rexpr, '(if rexpr rexpr rexpr)'):
        # this needs to determine if the true branch matches something where there is non-zero multiplicity on one of the branches
        env_prev = self.context
        condition_res = None
        try:
            self.context = self.context.copy()
            cond = self.apply(cond)
            cond = make_conjunction(self.context.local_to_rexpr(), cond)  # if there is an assignment, this should not get removed yet so unless it is already present in a higher context, then this needs to ignore this
            if isMultiplicity(cond):
                # check if the expression is like if(0,R,S) or if(1, R,S)
                if cond == 0: condition_res = False
                else: condition_res = True
            else:
                # check if the expression is like if(1+Q, R, S) in which case it is now true regardless of Q
                for ags in match(self, cond, '(+ any)'):
                    for a in ags:
                        if isMultiplicity(a):
                            assert a.get_argument(0) > 0
                            condition_res = True
                            break
        finally:
            self.context = env_prev


        if condition_res is True:
            return self.apply(true_r)
        elif condition_res is False:
            return self.apply(false_r)
        else:
            # return unmodified as this was unable to determine if the condition is true or false
            # TODO: we could return the updated condition
            # it is "possible" to rewrite the true/false branches, but that
            return rexpr

    return rexpr
##################################################


@register_rewrite('(plus args 3)')
def plus(self, rexpr):
    for a,b,c in match(self, rexpr, '(plus ground ground ground)'):
        # then this will match the ground values
        return multiplicity(1 if a+b == c else 0)
    for a,b, vc in match(self, rexpr, '(plus ground ground var)'):
        return Term('=', (vc, a+b))
    for a, vb, c in match(self, rexpr, '(plus ground var ground)'):
        return Term('=', (vb, c-a))
    for va, b, c in match(self, rexpr, '(plus var ground ground)'):
        return Term('=', (va, c-b))

    # return unchanged in the case that nothing matches
    return rexpr

@register_rewrite('(times args 3)')
def times(self, rexpr):
    for a,b,c in match(self, rexpr, '(times ground ground ground)'):
        return multiplicity(1 if a*b == c else 0)
    for a,b, vc in match(self, rexpr, '(times ground ground var)'):
        return Term('=', (vc, a*b))
    for a, vb, c in match(self, rexpr, '(times ground var ground)'):
        return Term('=', (vb, c/a))
    for va, b,c in match(self, rexpr, '(times var ground ground)'):
        return Term('=', (va, c/b))
    return rexpr

@register_rewrite('(min args 3)')
def min_rr(self, rexpr):
    for a,b,c in match(self, rexpr, '(min ground ground ground)'):
        return multiplicity(1 if min(a,b) == c else 0)
    for a,b,vc in match(self, rexpr, '(min ground ground var)'):
        return Term('=', (vc, min(a,b)))
    return rexpr

@register_rewrite('(max args 3)')
def max_rr(self, rexpr):
    for a,b,c in match(self, rexpr, '(min ground ground ground)'):
        return multiplicity(1 if max(a,b) == c else 0)
    for a,b,vc in match(self, rexpr, '(max ground ground var)'):
        return Term('=', (vc, max(a,b)))
    return rexpr


# technically these operations need to have an additional argument which is for
# it to always be returned true, as if the expression is `False is (a == b)`
# then it would be a not equal expressions

@register_rewrite('(lessthan args 2)')
def lessthan(self, rexpr):
    for a,b in match(self, rexpr, '(lessthan ground ground)'):
        return multiplicity(1 if a < b else 0)

    return rexpr


@register_rewrite('(lessthan_eq args 2)')
def lessthan_eq(self, rexpr):
    for a,b in match(self, rexpr, '(lessthan_eq ground ground)'):
        return multiplicity(1 if a <= b else 0)

    return rexpr

@register_rewrite('(equals args 2)')
def equals_rr(self, rexpr):
    for a,b in match(self, rexpr, '(equals ground ground)'):
        return multiplicity(1 if a == b else 0)
    return rexpr

# there is also range, not, not equal, abs etc which are not included



####################################################################################################

generated_var_cnt = 0
def generate_var():
    global generated_var_cnt
    generated_var_cnt += 1
    return Variable(f'$VAR_{generated_var_cnt}')

@track_rexpr_constructed
def uniquify_variables(rexpr, mapping=None):
    if not isinstance(rexpr, Term):
        return rexpr
    if mapping is None: mapping = {}
    return uniquify_variables_rec(rexpr, mapping)


def uniquify_variables_rec(rexpr, mapping):
    if not isinstance(rexpr, Term):
        return rexpr
    if isVariable(rexpr):
        # if the variable is contained in the expression, then this is going to give it a new name
        return mapping.get(rexpr, rexpr)
    elif rexpr.name == 'proj' and rexpr.arity == 2:
        var = rexpr.get_argument(0)
        rxp = rexpr.get_argument(1)
        new_var = generate_var()
        old = mapping.get(var)
        mapping[var] = new_var
        rxp = uniquify_variables_rec(rxp, mapping)
        mapping[var] = old
        return Term('proj', (new_var, rxp))
    elif rexpr.name == 'aggregator' and rexpr.arity == 4:
        op, resulting, incoming, rxp = rexpr.arguments
        new_var = generate_var()
        assert resultin != incoming  # this needs to be handled differently
        resulting = mapping.get(resulting, resulting)  # do the remapping for the returned variable
        old_var = mapping.get(incoming)
        mapping[incoming] = new_var
        rxp = uniquify_variables_rec(rxp, mapping)
        mapping[incoming] = old_var
        return Term('aggregator', (op, resulting, new_var, rxp))
    else:
        # this should attempt to rewrite all of the arguments of the term
        ret = []
        for v in rexpr.arguments:
            ret.append(uniquify_variables_rec(v, mapping))
        ret = Term(rexpr.name, ret)
        if ret != rexpr:
            return ret
        else:
            # avoid duplicating this if there are no changes
            return rexpr


def walk_rexpr(rexpr, func):
    # match against the R-exprs which have nested expressions
    def w(r):
        func(r)
        if r.name in ('+', '*'):
            for a in r.arguments: w(a)
        elif r.name == 'proj' and r.arity == 2:
            w(r.get_argument(1))
        elif r.name == 'aggregate' and r.arity == 4:
            w(r.get_argument(3))
    w(rexpr)

def contains_variable(rexpr, var):
    assert isVariable(var)
    if rexpr == var:
        return True
    if isinstance(rexpr, Term):
        for a in rexpr.arguments:
            if contains_variable(a, var): return True
    return False
    # found = False
    # def walker(rx):
    #     nonlocal found
    #     for a in rx.arguments:
    #         if a == var: found = True
    # walk_rexpr(rexpr, walker)
    # return found

def contains_any_variable(rexpr):
    if isVariable(rexpr): return True
    if isinstance(rexpr, Term):
        for a in rexpr.arguments:
            if contains_any_variable(a): return True
    return False

def replace_term(expr, mapping):
    if expr in mapping:
        return mapping[expr]
    elif isinstance(expr, Term):
        ret = []
        did_change = False
        for a in expr.arguments:
            n = replace_term(a, mapping)
            if n != a: did_change = True
            ret.append(n)
        if did_change:
            return Term(expr.name, ret)
        return expr  # return unmodified if nothing changes
    else:
        return expr

class IdentityWrapper:
    def __init__(self, v): self.v = v
    def __eq__(self, o): return isinstance(IdentityWrapper, o) and self.v is o.v
    def __hash__(self): return id(self.v)

def replace_identicial_term(expr, mapping):
    # there needs to be a method for replacing an expression which is a particular instances, using the IS expression
    # though if something is duplicated more than once in the expression (as these are read only pointers already), then that might
    # cause a particular problem?  I suppose that this would require a particular path through the expression, or we could just
    # make local copies throughout the expression such that unified expressions are distinct in memory
    i = IdentityWrapper(expr)
    if i in mapping:
        return mapping[i]
    elif isinstance(expr, Term):
        ret = []
        did_change = False
        for a in expr.arguments:
            n = replace_identicial_term(a, mapping)
            if n is not a: did_change = True
            ret.append(n)
        if did_change:
            return Term(expr.name, ret)
        return expr  # return unmodified if nothing changes
    else:
        return expr


####################################################################################################



def main():
    #rexpr = Term('plus', (1,2,Variable('x')))

    #rexpr = Term('aggregator', ('sum', Variable('x'), Variable('y'), Term('+', (Term('=', (Variable('y'), 7)), Term('=', (Variable('y'), 10))))  ))  # (X=sum(Y, (Y=7)))

    # rexpr = Term('if', (Term('=', (Variable('x'), 3)), Term('plus', (1,Variable('x'), Variable('y'))), Term('plus', (4,Variable('x'), Variable('y')))))

    # rexpr = Term('*', (Term('=', (Variable('x'), 4)) , rexpr))

    ctx = RewriteContext()
    simplify = RewriteEngine()

    rewrites.define_user_rewrite(
        'fib', 2,
        make_disjunction(
            make_conjunction(Term('=', (Variable(0), 0)), Term('=', (Variable(1), 0))),
            make_conjunction(Term('=', (Variable(0), 1)), Term('=', (Variable(1), 1))),
            make_project(
                'sub1', 'sub2', 'res1', 'res2',
                make_conjunction(
                    Term('lessthan', (1, Variable(0))),  # var_0 > 1
                    Term('plus', (Variable('sub1'), 1, Variable(0))),  # var_0 - 1
                    Term('plus', (Variable('sub2'), 2, Variable(0))),  # var_0 - 2
                    Term('fib', (Variable('sub1'), Variable('res1'))),  # res1 is fib(var_0 - 1)
                    Term('fib', (Variable('sub2'), Variable('res2'))),  # res2 is fib(var_0 - 2)
                    Term('plus', (Variable('res1'), Variable('res2'), Variable(1)))
                ))
        )
    )


    rexpr = Term('fib', (10, Variable('res')))

    print('Original R-expr:', '-'*50)
    print(rexpr.stylized_rexpr())



    for step in range(16):
        print('step:', step,'-'*50)
        rexpr = simplify.rewrite_once(rexpr)
        print(rexpr.stylized_rexpr())


    # print(rexpr.stylized_rexpr())
    # print('-'*50)

    # r = simplify.rewrite_once(rexpr)

    # print(r.stylized_rexpr())
    # print('-'*50)
    # r = simplify.rewrite_once(r)

    # print(r.stylized_rexpr())

if __name__ == '__main__':
    main()
