"""
Program transformations for supporting prefix aggregators.
"""
from dyna.syntax.generic import Rule, Term, FVar
from dyna.syntax.aggregators import AGG
from dyna.syntax.util import colors

# TODO: sometimes we generate the tmp-value rule (e.g., equations involving the
# value function or action-value function in MDPs tend need both - if the user
# has two equations with prefix aggrs they will generate rules for Q|V or V|Q
# multiple times.)
def handle_disjunctions(r):

    cs = []
    gs = []

    def run(x):
        assert not isinstance(x, Rule)

        if isinstance(x, (tuple, list)):
            return [run(a) for a in x]

        elif isinstance(x, FVar) or not isinstance(x, Term):
            # Variable or constant
            return x

        elif x.fn == '*' and x.arity == 0:  # gensym
            g = object()   # create a distinct object, which we can replace later.
            gs.append(g)
            return g

        # TODO: this is brittle
        elif x.fn == 'unless' and x.arity == 2:
            [_, result, condition] = x
            ctx = AggrCtx('|=', run([False] + list(flatten_disjuncts(condition))))
            cs.append(ctx)
            return Term(',', Term('!', ctx), run(result))

        elif x.fn == ';' and x.arity == 2:
            ctx = AggrCtx(':-', run(list(flatten_disjuncts(x))))
            cs.append(ctx)
            return ctx

        elif x.fn in AGG and len(x.args) == 1:
            # peek under to see if there is a semicolon
            aggr, expr = x
            ctx = AggrCtx(aggr, run(list(flatten_disjuncts(expr))))
            cs.append(ctx)
            return ctx

        else:
            return x.apply(run)

    h = run(r.head)
    x = run(r.value)

    rules = [Rule(h, r.aggr, x, [])]
    for c in cs:
        v = set(all_fvars_outside(c, [h, x]))
        head = Term(gen_functor(), *list(sorted(v & set(all_fvars(c.disj)))))
        for e in c.disj:
            rules.append(Rule(head, c.aggr, e, []))
        c.replacement = head

    for r in rules:
        for c in cs:
            [r.head, r.value] = replace_in_expr([r.head, r.value], c, c.replacement)
        for g in gs:
            [r.head, r.value] = replace_in_expr([r.head, r.value], g, make_gensym(r))

    return rules


def flatten_disjuncts(x):
    """
    >>> list(flatten_disjuncts(term('a ; (b ; (c; f) * 5) ; d')))
    [a, b, ((c ; f) * 5), d]
    """
    if isinstance(x, Term) and x.fn == ';' and x.arity == 2:
        [_, a, b] = x
        yield from flatten_disjuncts(a)
        yield from flatten_disjuncts(b)
    else:
        yield x


def make_gensym(r):
    return Term('&', Term(gen_functor(), *list(sorted(set(all_fvars([r.head, r.value]))))))


def gen_functor():
    gen_functor._i += 1
    return f'$gen{gen_functor._i}'
gen_functor._i = 0


class AggrCtx:
    def __init__(self, aggr, disj):
        assert isinstance(disj, list)
        self.aggr = aggr
        self.disj = disj
        self.replacement = None
    def __repr__(self):
        return f'{{AggrCtx: {colors.light.yellow % self.aggr} {self.disj}}}'


def all_fvars(x):
    if isinstance(x, FVar):
        yield x
    elif isinstance(x, (list,tuple,Term)):
        for y in x:
            yield from all_fvars(y)


def replace_in_expr(expr, target, replacement):
    if expr is target:
        return replacement
    elif isinstance(expr, (list, tuple, Term)):
        args = [replace_in_expr(x, target, replacement) for x in expr]
        return Term(*args) if isinstance(expr, Term) else args
    else:
        return expr


def all_fvars_outside(ctx, x):
    if isinstance(x, FVar):
        yield x
    elif x is ctx:   # do not enter this context.
        pass
    elif isinstance(x, AggrCtx):
        yield from all_fvars_outside(ctx, x.disj)
    elif isinstance(x, (list, tuple, Term)):
        for y in x:
            yield from all_fvars_outside(ctx, y)
