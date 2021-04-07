# a simple as possible implementation of R-exprs rewriting engine
# the term

import sys
import os

####################################################################################################
# utility functions

def parse_sexp(sexp):
    # from https://gist.github.com/pib/240957
    from string import whitespace
    atom_end = set('()"\'') | set(whitespace)
    stack, i, length = [[]], 0, len(sexp)
    while i < length:
        c = sexp[i]

        reading = type(stack[-1])
        if reading == list:
            if   c == '(': stack.append([])
            elif c == ')':
                stack[-2].append(stack.pop())
                if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
            elif c == '"': stack.append('')
            elif c == "'": stack.append([('quote',)])
            elif c in whitespace: pass
            else: stack.append((c,))
        elif reading == str:
            if   c == '"':
                stack[-2].append(stack.pop())
                if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
            elif c == '\\':
                i += 1
                stack[-1] += sexp[i]
            else: stack[-1] += c
        elif reading == tuple:
            if c in atom_end:
                atom = stack.pop()
                if atom[0][0].isdigit(): stack[-1].append(eval(atom[0]))
                else: stack[-1].append(atom)
                if stack[-1][0] == ('quote',): stack[-2].append(stack.pop())
                continue
            else: stack[-1] = ((stack[-1][0] + c),)
        i += 1
    return stack.pop()


####################################################################################################


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
        return isinstance(other, Term) and \
            self.__hashcache == other.__hashcache and \
            self.__name == other.__name and \
            self.__arguments == other.__arguments

    def __str__(self):
        return self.name + '(' + ', '.join(map(str, self.arguments)) + ')'


def Variable(name):
    return Term('$VARIABLE', (name,))


class UnificationFailure(Exception):
    pass

from collections import defaultdict


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
        if r.name == '=' and r.arity == 2:
            # then we need to ad this to the index.  This should then this
            # should check if there is a consistency issue with the value that
            # is being set.  In which case this can return unification failure
            val = self.get_value(r.get_argument(0))
            if val is None:
                self._assign_index[r.get_argument(0)] = r
            else:
                if r.get_argument(1) != val:
                    raise UnificationFailure()
                return  # there is no need to store this as it is already set

        # so that we can find things like lessthan(A,B) by just looking under lessthan/2
        self._kind_index[(r.name, r.arity)].add(r)

        super().add(r)  # this is just tracking the standard r-expr term

    def __contains__(self, v):
        if super().__contains__(v):
            return True
        if self._parent is not None:
            return self._parent.__contains__(v)
        return False

    def copy(self):
        return RewriteContext(self)

    def get_value(self, variable):
        r = self._assign_index.get(variable)
        if r is None and self._parent is not None:
            return self._parent.get_value(variable)

    def get_all_kinds(self, name, arity):
        r = self._kind_index.get((name, arity), set())
        if self._parent is not None:
            r |= self.get_all_kinds(name, arity)
        return r


class RewriteEngine:

    def __init__(self, rewrites=None):
        if rewrites is None:
            rewrites = globals()['rewrites']
        self.rewrites = rewrites
        self.context = {}

    def apply(self, rexpr):
        name, arity = rexpr.name, rexpr.arity

        if (name, arity) in self.rewrites:
            res = self.rewrites[(name, arity)](self, rexpr)
            if res != rexpr:
                return res

        if name in self.rewrites:
            res = self.rewrites[name](self, context)
            if res != rexpr:
                return res

        for key, r in self.rewrites.items():
            if isinstance(key, str) and '(' in key:  # this can enable the more power pattern matching for the rewrites
                for _ in match(self, rexpr, key):
                    res = r(self, rexpr)
                    if res != rexpr:
                        return res

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

rewrites = {}

user_defined_rewrites = {}

def match(self, rexpr, pattern, *pattern_args):
    # the pattern should be something which returns the variable
    # I suppose that we could use a for loop and then this would return an iterator in the case that the pattern matches
    expr = parse_sexp(pattern)

    # this should go through and do the matching for the expression
    def rec(rexpr, pattern):
        name = pattern[0]
        if isinstance(name, tuple) and len(name) == 1: name = name[0]
        if rexpr.name != name:
            # then the matching has failed
            return None
        args = pattern[1:]
        ret = []
        for i, a in enumerate(args):
            if a == ('any',):
                # then we are going to match the remainder of the expression
                ret.append(args[i:])
                break
            if a == ('args',):
                num = args[i+1]
                if num == rexpr.arity:
                    return []  # then this matches, but this is not going to match with the values of these arguments
                else:
                    return None
            ri = rexpr.arguments[i]
            if a == ('var',):
                if ri.name == '$VARIABLE':
                    ret.append(ri)
                else:
                    return None  # fail match
            elif a == ('ground',):
                if isinstance(ri, Term) and ri.name == '$VARIABLE':
                    if self is not None:
                        val = self.get_value(ri)
                        if val is not None:
                            ret.append(val)
                        else:
                            return None
                    else:
                        return None # fail match
                else:
                    # if this is a ground valie, then we should just return that
                    # though if this is a nested r-expr how would that work
                    ret.append(ri)
            elif a == ('rexpr',):
                ret.append(ri)
            elif isinstance(a, list):
                # then this is a recursive nested expression
                r = rec(args[i], a)
                if r is None:
                    return None  # then the match has failed
                ret.append(r)
            else:
                assert False  # idk if there should be anything else
        return ret

    res = rec(rexpr, expr[0])
    if res is not None:
        yield res
    # if the result is an empty array then this should return the result
    # then there is nothing that matches here


def multiplicty(val):
    assert isinstance(val, int) and val >= 0
    return Term('$MUL', (val,))


def register_rewrite(pattern):
    p = parse_sexp(pattern)
    def f(func):
        rewrites[pattern] = func
    return f


@register_rewrite('(* any)')
def multipliy_base(self, rexpr):
    ret = []
    assigns = {}  # these will want to be sorted first into the expression
    mul = 1
    for r in rexpr.arguments:
        z = self(r)
        if isinstance(z, int):
            mul *= z
            if mul == 0: return mul  # this has hit the shortcut of reducing to nothing
        if z.name == '=' and z.arity == 2:
            assigns[z.get_argument(0)] = z.get_argument(1)
            assert False



@register_rewrite('(+ any)')
def add_base(self, rexpr):
    ret = []
    env_prev = self.context
    for r in rexpr.arguments:
        # this will need to merge the common environments together
        # which means that this will need
        self.context = env_prev.copy()
        r = self(r)
        ret.append((r, self.context))
    # this will need to determine what is the common sets of these elements.  From there it will


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






####################################################################################################



def main():
    rexpr = Term('plus', (1,2,Variable('x')))

    ctx = RewriteContext()
    simplify = RewriteEngine()
    r = simplify(rexpr)

    print(r)

if __name__ == '__main__':
    main()
