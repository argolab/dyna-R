# a simple as possible implementation of R-exprs rewriting engine
# the term

import sys
import os

from collections import defaultdict
from functools import cache


####################################################################################################
# utility functions

# @cache  # this will get called a number of times with the same argument, so just cache the result
# def parse_sexp(sexp):
#     # from https://gist.github.com/pib/240957
#     from string import whitespace
#     atom_end = set('()"\'') | set(whitespace)
#     stack, i, length = [[]], 0, len(sexp)
#     while i < length:
#         c = sexp[i]

#         reading = type(stack[-1])
#         if reading == list:
#             if   c == '(': stack.append([])
#             elif c == ')':
#                 stack[-2].append(stack.pop())
#                 if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
#             elif c == '"': stack.append('')
#             elif c == "'": stack.append([('quote',)])
#             elif c in whitespace: pass
#             else: stack.append((c,))
#         elif reading == str:
#             if   c == '"':
#                 stack[-2].append(stack.pop())
#                 if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
#             elif c == '\\':
#                 i += 1
#                 stack[-1] += sexp[i]
#             else: stack[-1] += c
#         elif reading == tuple:
#             if c in atom_end:
#                 atom = stack.pop()
#                 if atom[0][0].isdigit(): stack[-1].append(eval(atom[0]))
#                 else: stack[-1].append(atom)
#                 if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
#                 continue
#             else: stack[-1] = ((stack[-1][0] + c),)
#         i += 1
#     return stack.pop()


def parse_sexp(sexpr):
    # super basic s-expression parser
    sexpr = sexpr.replace('(', ' ( ').replace(')', ' ) ').split()  # lex the expression
    stack = [[]]
    for symbol in sexpr:
        if symbol == '(':
            stack.append([])  # push stack
        elif symbol == ')':
            val = tuple(stack.pop())
            stack[-1].append(val)
        else:
            stack[-1].append(symbol)
    assert len(stack) == 1  # otherwise the s-expr is ill-formed
    return tuple(stack[0])




####################################################################################################
# base functions for the core

class Term:

    __slots__ = ('__name', '__arguments', '__hashcache')

    def __init__(self, name, arguments):
        #assert all(not isinstance(a, Variable) for a in arguments) and isinstance(name, str)  # there is not a special symbol for variables in this version
        assert isinstance(name, str)
        self.__name = name
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable
        self.__hashcache = hash(self.name) ^ hash(self.__arguments)

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


def Variable(name):
    return Term('$VARIABLE', (name,))

def isVariable(x):
    return x.name == '$VARIABLE' and x.arity == 1


class _multiplictyTerm(Term):
    def __init__(self, val):
        assert isinstance(val, int) and val >= 0
        super().__init__('$MUL', (val,))
    def __add__(self, other):
        assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
        return type(self)(self.arguments[0] + other.arguments[0])
    def __mul__(self, other):
        assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
        return type(self)(self.arguments[0] * other.arguments[0])
    def __int__(self): return self.arguments[0]

def multiplicty(val):
    if isinstance(val, int):
        return _multiplictyTerm(val)
    if isinstance(val, _multiplictyTerm):
        return val
    assert False  # wtf
    # assert isinstance(val, int) and val >= 0
    # # if val == 0:
    # #     import ipdb; ipdb.set_trace()
    # return val
    # #return Term('$MUL', (val,))

def isMultiplicty(val):
    return isinstance(val, _multiplictyTerm)


##################################################

class UnificationFailure(Exception):
    pass

class RewriteContext(set):

    def __init__(self, parent=None):
        self._parent = parent
        self._assign_index = {}
        self._kind_index = defaultdict(set)

    def _build_assign_index(self):
        for r in self:
            if r.name == '=' and r.arity == 2:
                # if there are two values for the
                self._assign_index[r.get_argument(0)] = r

    def add_rexpr(self, r):
        if self.__contains__(r):
            # this is already tracked, sowe are not going to add it again to the expression
            return
        if r.name == '=' and r.arity == 2:
            # then we need to ad this to the index.  This should then this
            # should check if there is a consistency issue with the value that
            # is being set.  In which case this can return unification failure



            val = self.get_value(r.get_argument(0))
            if val is None:
                self._assign_index[r.get_argument(0)] = r
            else:
                if r.get_argument(1) != val:
                    import ipdb; ipdb.set_trace()
                    raise UnificationFailure()
                return  # there is no need to store this as it is already set

        # so that we can find things like lessthan(A,B) by just looking under lessthan/2
        self._kind_index[(r.name, r.arity)].add(r)

        super().add(r)  # this is just tracking the standard r-expr term

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
        assert False

    def __sub__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        assert False

    def get_value(self, variable):
        if isVariable(variable):
            r = self._assign_index.get(variable)
            if r is None and self._parent is not None:
                return self._parent.get_value(variable)
        else:
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


class RewriteEngine:

    def __init__(self, *, rewrites=None, kind='simple', context=None):
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

    def apply(self, rexpr):
        try:
            name, arity = rexpr.name, rexpr.arity

            # this tracks that we have seen this, assignments will be placed first in the expression so those should be made avaliable before
            # we encounter something that will make use of the expression.
            self.context.add_rexpr(rexpr)

            if (name, arity) in self.rewrites:
                res = self.rewrites[(name, arity)](self, rexpr)
                assert res is not None
                if res != rexpr:
                    return res

            if name in self.rewrites:
                res = self.rewrites[name](self, context)
                assert res is not None
                if res != rexpr:
                    return res

            for key, r in self.rewrites.items():
                if isinstance(key, str) and '(' in key:  # this can enable the more power pattern matching for the rewrites
                    for _ in match(self, rexpr, key):
                        res = r(self, rexpr)
                        assert res is not None
                        if res != rexpr:
                            return res
        except UnificationFailure:
            # then this will be a zero for these expressions
            return multiplicty(0)

        # there is no rewrite here which applies
        return rexpr

    def __call__(self, rexpr):
        return self.apply(rexpr)

    def get_value(self, rexpr):
        if rexpr.name == '$VARIABLE':
            name = rexpr.get_argument(0)
            return self.context.get(name) # this returns the value
        else:
            # this must be a constant value
            return variable


def fully_rewrite(rewrite_engine :RewriteEngine, rexpr):
    while True:
        old_rexpr = rexpr
        rexpr = rewrite_engine(rexpr)
        if old_rexpr == rexpr:  # meaning that there were no rewrites applied to the expression
            break

class RewriteCollection:
    """A class for tracking rewrite operators.  Operators are segmented such that they
    can be looked up quickly using the name of an R-expr or in the case that more advanced matching is required
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

    def do_user_defined_rewrite(self, rexpr):
        # this should take the name arity of a given expression and then subsuite in new names for the variables
        n = (rexpr.name, rexpr.arity)
        rxp = self.user_defined_rewrites[n]
        # this will have to subsuite in the variable names and create new variable names for the expression
        var_map = {}
        for i, new_name in enumerate(rexpr.arguments):
            var_map[Variable(i)] = new_name

        rxp = uniquify_variables(rxp, var_map)

        # this will have to replace all of the variables or create new variable names for all of the expressions
        # when this encounters something that is
        assert False

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
        for key, funcs in self.full_match.items():
            if match(context, rexpr, pattern):
                yield from funcs

        # in the case that the name/arity matches the user defined rewrite, then the generic user
        # rewrite handler will handle it
        if n in self.user_defined_rewrites:
            yield self.do_user_defined_rewrite

rewrites = {}

user_defined_rewrites = {}

def match(self :RewriteContext, rexpr, pattern, *pattern_args):
    # the pattern should be something which returns the variable
    # I suppose that we could use a for loop and then this would return an iterator in the case that the pattern matches

    # match might be outside
    expr = parse_sexp(pattern)

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
        # else:
        #     if rexpr.arity + 1 != len(pattern):
        #         # the arity of a longer term has failed to match
        #         return None

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
                term_idx = len(rexpr.arguments)
            elif n == 'args':
                num = int(pattern[match_idx + 1])
                match_idx += 1
                if rexpr.arity == num:
                    return []  # successful match
                else:
                    return None  # unsuccessful
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



    # # this should go through and do the matching for the expression
    # def rec(rexpr, pattern):
    #     name = pattern[0]
    #     if isinstance(name, tuple) and len(name) == 1: name = name[0]
    #     if name == 'OR':
    #         # then there are a few different things that could match here
    #         for child in pattern[1:]:
    #             res = rec(rexpr, child)
    #             if res is not None:
    #                 return res
    #         return  # then none of the OR branches matched, so this is going to
    #     elif name == 'ground':
    #         if isVariable(rexpr):
    #             if self is not None:
    #                 val = self.get_value(rexpr)
    #                 if val is not None:
    #                     return val
    #             else:
    #                  return None
    #         else:
    #             # this must be a ground value, so we can just return this
    #             return rexpr
    #     elif name == 'var':
    #         if isVariable(rexpr):
    #             return rexpr
    #         return  # fail match
    #     elif name == 'rexpr':
    #         return rexpr  # just match anything here
    #     elif isinstance(name, list) and name[0] == 'param':
    #         idx = name[1]
    #         if rexpr == pattern_args[idx]:
    #             return []  # successful match that returns nothing
    #         else:
    #             return None  # failed to match
    #     if rexpr.name != name:
    #         # then the matching has failed
    #         return None
    #     args = pattern[1:]
    #     ret = []
    #     for i, a in enumerate(args):
    #         if a == ('any',):
    #             # then we are going to match the remainder of the expression
    #             ret.append(args[i:])
    #             break
    #         if a == ('args',):
    #             num = args[i+1]
    #             if num == rexpr.arity:
    #                 return []  # then this matches, but this is not going to match with the values of these arguments
    #             else:
    #                 return None
    #         ri = rexpr.arguments[i]
    #         # if the pattern does not have the tuple around it
    #         rr  = rec(ri, a)
    #         if rr is None:
    #             return None  # the match failed
    #         else:
    #             ret += rr  # then the match return something that we want to accumulate

    #         # if isinstance(a, list) and a[0] == ('param',):
    #         #     # this for matching expressions like `(something (param 0) var (param 1))`
    #         #     idx = a[1]
    #         #     if ri == pattern_args[idx]:
    #         #         # match was successful, but don't bother returning it
    #         #         pass
    #         #     else:
    #         #         return None  # failed match
    #         # elif a == ('var',):
    #         #     if ri.name == '$VARIABLE':
    #         #         ret.append(ri)
    #         #     else:
    #         #         return None  # fail match
    #         # elif a == ('ground',):
    #         #     if isinstance(ri, Term) and ri.name == '$VARIABLE':
    #         #         if self is not None:
    #         #             val = self.get_value(ri)
    #         #             if val is not None:
    #         #                 ret.append(val)
    #         #             else:
    #         #                 return None
    #         #         else:
    #         #             return None # fail match
    #         #     else:
    #         #         # if this is a ground valie, then we should just return that
    #         #         # though if this is a nested r-expr how would that work
    #         #         ret.append(ri)
    #         # elif a == ('rexpr',):
    #         #     ret.append(ri)
    #         # elif isinstance(a, list):
    #         #     # then this is a recursive nested expression
    #         #     r = rec(args[i], a)
    #         #     if r is None:
    #         #         return None  # then the match has failed
    #         #     ret.append(r)
    #         # else:
    #         #assert False  # idk if there should be anything else
    #     return ret

    res = rec(rexpr, expr[0])
    if res is not None:
        if len(res) == 0:
            # then this is still a successful match, but returning an empty array will fail to do any looping
            # so we return an array which has size of >0 but there is no content
            return [None]
        if len(res) == 1:
            return res[0]
        else:
            return res


    # if the result is an empty array then this should return the result
    # then there is nothing that matches here
    # this function does not have to use an

    # this will always return something iterable, as this is used within the context of an for loop
    # but tihs will have that bool([]) == False, so it can also be used inside of a
    return []



def register_rewrite(pattern):
    return lambda x: x
    p = parse_sexp(pattern)
    def f(func):
        rewrites[pattern] = func
    return f


####################################################################################################
# Rewrites

@register_rewrite('(* any)')
def multipliy_base(self, rexpr):
    ret = []
    assigns = {}  # these will want to be sorted first into the expression
    mul = 1
    for r in rexpr.arguments:
        z = self(r)
        if isinstance(z, int):  # this is going to want to match the multiplicty for some value
            mul *= z
            if mul == 0: return mul  # this has hit the shortcut of reducing to nothing
        if z.name == '=' and z.arity == 2:
            assigns[z.get_argument(0)] = z.get_argument(1)
            assert False



@register_rewrite('(+ any)')
def add_base(self, rexpr):
    ret = []
    env_prev = self.context
    try:
        for r in rexpr.arguments:
            # this will need to merge the common environments together
            # which means that this will need
            self.context = env_prev.copy()
            r = self(r)
            ret.append((r, self.context))

            import ipdb; ipdb.set_trace()
    finally:
        # this will need to update the env_prev with whatever is the new content which
        self.context = env_prev
    # this will need to determine what is the common sets of these elements.  From there it will



@register_rewrite('(= args 2')
#@register_rewrite('(unify args 2)')
def unify(self, rexpr):
    for a,b in match(self, rexpr, '(= ground ground)'):
        return multiplicty(1 if a == b else 0)
    for a,vb in match(self, rexpr, '(= ground var)'):
        return Term('=', (vb, a))
    for va, b in match(self, rexpr, '(= var ground)'):
        return Term('=', (va, b))
    # if there is an ordering on the vraiable names, then we should consider that
    return rexpr

@register_rewrite('(structure ground any)')
def unify_structure(self, rexpr):
    # there are a variable number of arguments
    for name, res_variable, args in match(self, rexpr, '(structure ground ground any)'):
        # this needs to unpack the variables in the expression
        if res_variable.name != name or res_variable.arity != len(args):
            # this has failed to match the given expression
            return multiplicty(0)
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





@register_rewrite('(proj var rexpr)')
def proj(self, rexpr):
    for v, r in match(self, rexpr, '(proj var rexpr)'):
        rr = self(r)  # this is going to apply the values to the

    assert False

@register_rewrite('(aggregator ground var var rexpr)')
def aggregator(self, rexpr):
    operator = {
        'sum': sum,
        'prod': lambda a,b: a*b,
        'min': min,
        'max': max,
    }
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
        rxp = self(rxp)  # this should attempt to
        for _ in match(self, rxp, '(mul 0)'):
            return Term('=', (resulting, identity[op]))
        for res in match(self, rxp, '(= (param 0) ground)', incoming):
            return Term('=', (resulting, res))
        for ags in match(self, rxp, '(+ any)'):
            # this is a disjunction between many different variables
            # this is going to have to construct many projects and new variables
            intermediate_vars = [generate_var() for _ in range(len(ags))]
            nested_exprs = []
            additional_exprs = []
            for nr, nv in zip(ags, intermediate_vars):
                nested_exprs.append(Term('aggregator', (op, nr, incoming, nv)))
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

        # there are no rewrites which can be applied here
        return rexpr

##################################################


@register_rewrite('(plus args 3)')
def plus(self, rexpr):
    for _ in match(self, rexpr, '(plus any)'):
        for a,b,c in match(self, rexpr, '(plus ground ground ground)'):
            # then this will match the ground values
            if a+b == c:
                return multiplicty(1)
            else:
                return multiplicty(0)
        for a,b, vc in match(self, rexpr, '(plus ground ground var)'):
            return Term('=', (vc, a+b))
        for a, vb, c in match(self, rexpr, '(plus ground var ground)'):
            return Term('=', (vb, a-c))
        for va, b, c in match(self, rexpr, '(plus var ground ground)'):
            return Term('=', (va, b-c))

    # return unchanged in the case that nothing matches
    return rexpr

@register_rewrite('(times args 3)')
def times(self, rexpr):
    for a,b,c in match(self, rexpr, '(times ground ground ground)'):
        return multiplicty(1 if a*b == c else 0)
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
        return multiplicty(1 if min(a,b) == c else 0)
    for a,b,vc in match(self, rexpr, '(min ground ground var)'):
        return Term('=', (vc, min(a,b)))
    return rexpr

@register_rewrite('(max args 3)')
def max_rr(self, rexpr):
    for a,b,c in match(self, rexpr, '(min ground ground ground)'):
        return multiplicty(1 if max(a,b) == c else 0)
    for a,b,vc in match(self, rexpr, '(max ground ground var)'):
        return Term('=', (vc, max(a,b)))
    return rexpr




####################################################################################################

generated_var_cnt = 0
def generate_var():
    global generated_var_cnt
    generated_var_cnt += 1
    return f'$VAR_{generated_var_cnt}'

def uniquify_variables(rexpr, mapping=None):
    if not isinstance(rexpr, Term):
        return rexpr
    if mapping is None: mapping = {}
    if isVariable(rexpr):
        # if the variable is contained in the expression, then this is going to give it a new name
        return mapping.get(rexpr, rexpr)
    elif rexpr.name == 'proj' and rexpr.arity == 2:
        var = rexpr.get_argument(0)
        rxp = rexpr.get_argument(1)
        new_var = generate_var()
        old = mapping.get(var)
        mapping[var] = new_var
        rxp = uniquify_variables(rxp, mapping)
        mapping[var] = old
        return Term('proj', (new_var, rxp))
    elif rexpr.name == 'aggregator' and rexpr.arity == 4:
        op, resulting, incoming, rxp = rexpr.arguments
        new_var = generate_var()
        assert resultin != incoming  # this needs to be handled differently
        resulting = mapping.get(resulting, resulting)  # do the remapping for the returned variable
        old_var = mapping.get(incoming)
        mapping[incoming] = new_var
        rxp = uniquify_variables(rxp, mapping)
        mapping[incoming] = old_var
        return Term('aggregator', (op, resulting, new_var, rxp))
    else:
        # this should attempt to rewrite all of the arguments of the term
        ret = []
        for v in rexpr.arguments:
            ret.append(uniquify_variables(v, mapping))
        ret = Term(rexpr.name, ret)
        if ret != rexpr:
            return ret
        else:
            # avoid duplicating this if there are no changes
            return rexpr


####################################################################################################



def main():
    #rexpr = Term('plus', (1,2,Variable('x')))

    rexpr = Term('aggregator', ('sum', Variable('x'), Variable('y'), Term('+', (Term('=', (Variable('y'), 7)), Term('=', (Variable('y'), 10))))  ))  # (X=sum(Y, (Y=7)))

    ctx = RewriteContext()
    simplify = RewriteEngine()
    r = simplify(rexpr)

    print(r)

if __name__ == '__main__':
    main()
