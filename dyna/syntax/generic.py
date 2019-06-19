"""
Generic term representation for Dyna based on Prolog.
"""

# Richer term representations
#  - Types as Prolog functions
#  - Tree automata representations for sets of terms
#  - Prolog terms with primitively typed variables (e.g., `f(X:int)`), very
#    similar to delayed constraints
#
# Prolog terms are insufficient for default reasoning:
# - Prolog terms are
#   - not closed under union (e.g., we can't represent `f(X) | g(1)`).
#   - not closed under negation
#
# Default reasoning requires intersection and set subtraction.
#
# Terms with bounded depth equality seem to (decidably) support these
# operations.

# TODO: Var should be an instance of Term (or some other common data type)

from functools import total_ordering
from copy import deepcopy

#null_ptr = 1234567890 #object()
#null_ptr = object()    # Why doesn't this work?
class null_ptr: pass


def has_null(x):
    x = deref(x)
    if x is None:
        return True
    elif isinstance(x, Term):
        return any(map(has_null, x))


def has_err(x):
    x = deref(x)
    if isinstance(x, Term):
        if x.fn == '$error':
            return True
        else:
            return any(map(has_err, x))


# TODO: hashable throws away any covariance in the Term, e.g., `f(X,X) ==>
# f(none, none)`... This is less than ideal.  I think we can get around it.
def hashable(k):
    "Map a `Term` into something hashable. (Free variables get mapped to `None`)."
    if isinstance(k, RVar):
        if not k.isbound:
            return None
        else:
            return hashable(k.value)
    elif isinstance(k, Term):
        return k.apply(hashable)
    else:
        return k


def all_vars(x):
    x = deref(x)
    if isinstance(x, RVar):
        yield x.root()
    elif isinstance(x, Seq):
        for y in x:
            yield from all_vars(y)


def unifies(A, B):
    "Intersection"
    u = False
    for _ in unify(A, B):
        u = True
    return u


def same(x, y):
    "Set equivalence"
    return covers(x, y) and covers(y, x)


def intersect(x, y):
    "Create a fresh term that represents the interesection of terms `x` and `y`."
    z = None
    for _ in unify(x, y):
        z = deepcopy(deref(x))
    return z


@total_ordering
class RVar:
    "Runtime variable"
    def __init__(self, name):
        assert isinstance(name, str)
        self.name = name         # the name is optional; only used for pretty-printing
        self.value = null_ptr
        #self.constraints = []

    def root(self):
        root = self
        while isinstance(root.value, RVar):
            root = root.value
        return root

    # TODO: We can schedule constraint solving/checking differently than we do
    # here.  We don't have a very specific architecture for RL to control in
    # mind yet.  However, to give a sense of the flexibility we have, I believe
    # that wWe are only forced to `solve the constraints` when we consolidate a
    # stream of results.
    #
    # TODO: If we had a constraint-logic programming system, the operation we
    # would be doing would not just be *checking* the constraints -- rather, we
    # would be *solving* the constraints -- much like unification *solves* a
    # family of equations over expression trees.
    #
    # Constraints are accumulated at the root of the unification tree.  Note: We
    # must be careful always call "check constraints" on the root: it is the
    # only place where the constraints of *all* of the unified variables are
    # available.
    def unify(self, x):
        if self.isbound:
            yield from unify(self.value, x)

        elif isinstance(x, RVar) and x.isbound:
            yield from unify(self, x.value)

        elif not self.occurs_check(x):
            was = self.value

            # Suppose that we have unified `X` against a complicated term
            # structure, which contians another variable `Y`
            #
            #    `f(... g( ... Y ... ) ...))`
            #
            #    the we need to notify X (or its root) if Var changes.
            #
            # (note that when strucure is present `X=/=Y` thanks to the occurs
            # check, but when there is no structure we may have X=Y).

            #vs = list(all_vars(x))
            #vs.append(self)
            #with Unification(vs):
            #    self.value = x
            #    if self.check_constraints():
            #        yield

            self.value = x
            yield

            self.value = was

#    def check_constraints(self):
#        # TODO: make sure this loop isn't doing more work than necessary.
#        done = set()
#        agenda = self.constraints[:]
#        while agenda:
#            c = agenda.pop()
#            if not c(): return False
#            if c in done: continue
#            done.add(c)
#            for v in c.variables:
#                agenda.extend(v.constraints)
#        return True

    def occurs_check(self, x):
        """
        Return true if variable var occurs anywhere in x
        (or in subst(s, x), if s has a binding for x).
        """

        if self is x:
            return True
        elif isinstance(x, RVar) and x.isbound:
            return self.occurs_check(x.value)
        elif isinstance(x, Seq):
            return any(self.occurs_check(e) for e in x)
        else:
            return False

    def __repr__(self):
        if self.isbound:
            return pp(self.value)
        else:
            return self.name

    @property
    def isbound(self):
        return self.value is not null_ptr

    def __hash__(self):
        if self.isbound:
            return hash(self.value)
        else:
            # TODO: lookup hashing algorithm used in prolog.
            #raise AssertionError("Can't hash nonground terms.")
            return object.__hash__(self)

    def __eq__(self, other):
        x = deref(self)
        y = deref(other)
        if isinstance(x, RVar) and isinstance(y, RVar):
            assert x.value is null_ptr and y.value is null_ptr
            return x is y

        elif isinstance(x, RVar) and not isinstance(y, RVar):
            return False

        elif not isinstance(x, RVar) and isinstance(y, RVar):
            return False

        else:
            return x == y

    def __lt__(self, other):
        if not isground(self) or not isground(other):
            return str(self) < str(other)
        return lt(self.value, other)


def rgensym():
    rgensym.i += 1
    return RVar(f'$Gen{rgensym.i}')
rgensym.i = 0


class FVar:
    def __init__(self, name):
        assert isinstance(name, str)
        self.name = name

    def __repr__(self):
        return f'{self.name}'

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        return isinstance(other, FVar) and self.name == other.name

    def __lt__(self, other):
        return self.name < other.name


def fgensym():
    fgensym.i += 1
    return FVar(f'$Gen{fgensym.i}')
fgensym.i = 0


def unify(x, y):
    "Attempt to unify `x` with `y`."
    if x == y:
        yield
    elif isinstance(x, RVar):
        yield from x.unify(y)
    elif isinstance(y, RVar):
        yield from y.unify(x)
    elif isinstance(x, Seq) and isinstance(y, Seq) and len(x) == len(y):
        yield from unifyN(x, y)


def unifyN(xs, ys):
    "Attempt to unify two sequences, `xs` and `ys`, assumed to have equal length."
    assert len(xs) == len(ys)
    if len(xs) == 0:
        yield
    else:
        [x,*xs], [y,*ys] = xs, ys
        for _ in unify(x, y):
            yield from unifyN(xs, ys)


if 0:
    def covers(x, y):
        "Subset"
        covered = False
        for _ in cover(x, y):
            covered = True
        return covered


    def cover(x, y):
        "Helper method for `covers(x,y)`."
        x = deref(x); y = deref(y)

        if x == y:
            yield

        elif isinstance(x, RVar):

            if not x.occurs_check(y):
                was = x.value
                x.value = y
                yield
                x.value = was

        elif isinstance(x, Seq) and isinstance(y, Seq) and len(x) == len(y):
            [x, *xs], [y, *ys] = x, y
            for _ in cover(x, y):
                yield from cover(xs, ys)

else:
    def covers(x, y):
        """
        Find a substitution `s` such that `subst(x, s) == y`.  If no such
        substitution exists, return `None`.
        """
        return _cover(x, y, {}) is not None

    def _cover(x, y, s):
        "Helper method for `covers(x,y)`. Note: calls mutate `s`."
        x = deref(x); y = deref(y)
        if x == y:
            return s

        elif isinstance(x, RVar):
            if x in s:
                if s[x] == y:
                    return s
            else:
                s[x] = y            # Note: we do *not* need to copy `s`.
                return s

        elif isinstance(x, Term) and isinstance(y, Term) and x.arity == y.arity:
            for a, b in zip(x.fargs, y.fargs):
                s = _cover(a, b, s)
                if s is None: return
            return s


#def subst(x, s):
#    if isinstance(x, RVar):
#        if x in s:
#            return x[s]
#        else:
#            return x
#    elif isinstance(x, Term):
#        return y.apply(lambda y: subst(y, s))
#    else:
#        return x


def generalizer(t1, t2):
    "Anti-unification algorithm"
    rgensym.i = 0

    s1 = {}
    s2 = {}

    def traverse(x, y):
        "Helper method for `covers(x,y)`. Note: calls mutate `s`."

        if x == y:
            return x

        elif isinstance(x, RVar):   # x != y and x is a variable

            if x.occurs_check(y):
                if x not in s1:
                    s1[x] = rgensym()
                return s1[x]

            if x in s1:             # already have an subst for `x`, it must match.
                if s1[x] != y:
                    s1[x] = rgensym()
                    return s1[x]
            else:
                s1[x] = y            # Note: we do *not* need to copy `s`.
            return x

        elif isinstance(y, RVar):   # x != y, x is not a variable, y is a variable

            if y.occurs_check(x):
                if y not in s2: s2[y] = rgensym()
                return s2[y]

            if y in s2:
                if s2[y] != x:
                    s2[y] = rgensym()
                    return s2[y]
            else:
                s2[y] = x            # Note: we do *not* need to copy `s`.
            return y

        elif isinstance(x, Term) and isinstance(y, Term) and len(x) == len(y):
            return Term(*(traverse(a, b) for a, b in zip(x, y)))

        else:
            if x in s1:
                return s1[x]
            if y in s2:
                return s2[x]

            z = rgensym()
            s1[x] = z
            s2[y] = z
            return z

    return traverse(t1, t2)


# Implementation note: `deref` is not a method on `Var` and `Term` because we
# might call it on other types such as int and str.
def deref(x):
    "Recursive pointer snapping on variables."

    if isinstance(x, RVar):
        if x.isbound:                  # x is a free variable.
            return deref(x.value)
        else:
            return x

    elif isinstance(x, Term):
        return x.apply(deref)

    elif isinstance(x, list):
        return [deref(y) for y in x]

    elif isinstance(x, tuple):
        return tuple(deref(y) for y in x)

    else:
        return x


@total_ordering
class Term:

    def __init__(self, *fargs):
        self.fargs = fargs

        # TODO: phase out the special status of fn
        self.fn, *args = self.fargs
        self.args = tuple(args)

    def apply(self, f):
        return Term(*map(f, self))

    @property
    def arity(self):
        return len(self)-1

    def __len__(self):
        return len(self.fargs)

    def __repr__(self):
        return pp(self)

    def __iter__(self):
        return iter(self.fargs)

    # TODO: Create `ImmutableTerm`, which is required for hashing.  This will
    # help avoid bugs due to in-place mutation (e.g., the one we saw in cycle
    # detection.)
    def __hash__(self):
        return hash(self.fargs)

    def __eq__(self, other):
        other = deref(other)
        return isinstance(other, Term) and self.fargs == other.fargs

    def __lt__(self, other):
        if isinstance(other, Term):
            return lt(self.fargs, other.fargs)
        else:
            return lt(self.fn, other)


Seq = (Term, list, tuple)


def lt(a, b):
    try:
        return a < b
    except TypeError:
        return type(a).__name__ < type(b).__name__


def isground(x):
    if isinstance(x, RVar):
        return x.value is not null_ptr and isground(x.value)
    elif isinstance(x, Seq):
        return all(map(isground, x))
    else:
        assert not isinstance(x, FVar)
        return True


# TODO: remove unnecessary parens.
# TODO: write pretty printer in Dyna and put it in builtins (a type of `$sty`).
def pp(x):
    x = deref(x)

    # if we don't know the functor there isn't much we can do.
    if x is None:
        return '$null'

    elif isinstance(x, bool):
        return str(x).lower()   # booleans and null are lower case

    elif isinstance(x, str):
        x = repr(x)    # should be a double-quoted string, unlike Python
        x = f'"{x[1:-1]}"'   # TODO: need to redo escapes
        return x

    elif isinstance(x, Term):
        return pp_term(x)

    else:
        return repr(x)


def pp_term(x):
    from dyna.syntax.aggregators import AGG

    if isinstance(x.fn, FVar) or not isground(x.fn):
        # use $term(X, ...) when the functor is not ground.
        return f'$term({", ".join(pp(y) for y in x)})'

    if x.arity == 0:
        if x.fn == '$nil':
            return pp_list(x)
        else:
            return pp_functor(x.fn)

    elif x.arity == 1:
        if x.fn in AGG:
            return f'({x.fn} {pp(x.args[0])})'
        else:
            return pp_term_default(x)

    elif x.arity == 2:
        assert not isinstance(x.fn, (RVar, FVar))
        # Handle some overrides

        if x.fn == ',':
#            return f"','({', '.join(pp(x) for x in x.fargs[1:])})"
            return f"({', '.join(pp(x) for x in x.fargs[1:])})"

        elif x.fn == 'is':
            # The default display is spaces around the infix operator
            [_, a, b] = x
            return f'({pp(b)} ↦ {pp(a)})'

        elif x.fn == '$cons':    # pretty-print `$cons/2` lists.
            return pp_list(x)

        else:

            infix = {
                # is/in/with_key are infix, despite being alpha-numeric
                'is': ' is ',
                'in': ' in ',
                'with_key': ' with_key ',
                # comma and colon have different spacing
                ',': ', ',
                ':': ': ',
            }.get(x.fn)

            if (not str(x.fn)[0].isalpha() and not x.fn.startswith('$')) or infix:
                # The default display is spaces around the infix operator
                infix = infix or f' {x.fn} '
                [_, a, b] = x
                return f'({pp(a)}{infix}{pp(b)})'

            else:
                return pp_term_default(x)
    else:
        return pp_term_default(x)


def pp_term_default(x):
    [fn, *args] = x
    assert isinstance(fn, str), [type(fn), fn, args]
    if fn == '&' and len(args) == 1:
        return f'&{pp(args[0])}'
    return f'{pp_functor(fn)}({", ".join(pp(x) for x in args)})'


import re
def pp_functor(x):
    y = str(x)
    # the pattern below should match the pattern for `FUNCTOR1` in the front-end parser.
    if not re.match("^([$]?[a-zαβγδεζηθικλμνξοπρστυφχψω]['a-zA-Z0-9_αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]*)$", x):
        y = f"'{y}'"
    return y


def pp_list(x):
    xs = []
    y = x
    while True:
        if isinstance(y, Term) and y.fn == '$nil' and y.arity == 0:
            return f'[{", ".join(pp(x) for x in xs)}]'
        elif isinstance(y, Term) and y.fargs[0] == '$cons':
            [_,a,y] = y.fargs
            xs.append(a)
        else:
            return f'[{", ".join(pp(x) for x in xs)} | {y}]'


# TODO: make `Rule` a $rule(Index,Head,Aggr,Value,Sides) term, where all
# variables are frozen.
class Rule:
    def __init__(self, head, aggr, value, sides, index=None, **metadata):
        assert isinstance(aggr, str), aggr
        self.head = head
        self.aggr = aggr
        self.value = value
        self.sides = sides
        self.index = index
        self.metadata = metadata

    def __iter__(self):
        return iter([self.head, self.aggr, self.value, self.sides])

    def __hash__(self):
        return hash(self.astuple())

    def __eq__(self, other):
        return self.astuple() == other.astuple()

    def astuple(self):
        return (self.head, self.aggr, self.value, self.sides, self.index)

    def __repr__(self):
        h,_,v,_ = map(pp, self)
        if self.sides:
            return f'{h} {self.aggr} {v} for {", ".join(map(pp, self.sides))}.'
        return f'{h} {self.aggr} {v}.'


def fresh(z):
    """
    Allocate fresh variables in Rules, Terms, etc.
    """

    m = {}    # mapping from original old var to fresh var

    def helper(x):
        assert not isinstance(x, RVar)
        if isinstance(x, FVar):
            if x not in m:         # already refreshed
                # TODO: We are currently assuming that all variables passed to
                # Freshener are FVar -- i.e., syntactic variables, which means
                # that they are unbound when we call Freshener, but this is not
                # a requirement.  We collapse the FVar and RVar distinction by
                # simply copying values/pointer here.  This will allow use to
                # perform constant unifications upfront.
                m[x] = RVar(x.name)    # use the original name
            return m[x]

        elif isinstance(x, Rule):
            # freshen all variables in the rule so that we can safely use them without
            # risk of clashing with variables already in use. (Unfortunately, we can't
            # freshen the rule up-front because rule may be used more than once)
            return Rule(*map(helper, x), index = x.index)

        elif isinstance(x, Term):
            return x.apply(helper)

        elif isinstance(x, (tuple, list)):
            return [helper(a) for a in x]

        else:
            # constants
            return x

    return helper(z)


# XXX: Item technically means something else.
Item = (Term, RVar)
