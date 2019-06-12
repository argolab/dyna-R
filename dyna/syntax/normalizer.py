from dyna.syntax.generic import Term, FVar, Rule
from dyna.syntax.aggregators import AGG
from dyna.syntax.syntax import term, run_parser

from dyna.interpreter import VariableId, constant
from dyna import BuildStructure
from dyna.context import dyna_system
from dyna.syntax.util import colors

from dyna import Frame



# linearization pass
# `call_term` instead of `is`
# `build_structure` instead unify


def normalize(x):
    """
    Normalize `x` into a form where all intermediate queries have a value slot.
    """

    xs = []
    n = 0

    def genvar():
        nonlocal n
        n += 1
        return FVar(f'$V{n}')

    def run(x, unif=False):
        assert not isinstance(x, Rule)

        if isinstance(x, (tuple, list)):
            return [run(a) for a in x]

        elif isinstance(x, FVar) or not isinstance(x, Term):
            # Variable or constant
            if isinstance(x, FVar):
                return VariableId(x)
            else:
                return constant(x)

        elif x.fn == '&' and x.arity == 1:
            [_, a] = x.fargs
            return run(a, unif=True)

        elif x.fn == '*' and x.arity == 1:
#            [_, a] = x.fargs
#            v = genvar()
#            xs.append(Term('is', v, run(a)))
#            return v
            raise NotImplementedError('indirect evaluation not yet supported')

        elif x.fn == 'is' and x.arity == 2:
            [_,v,k] = x
            if isinstance(k, FVar):
                #k = Term('*', k)
                raise NotImplementedError('indirect evaluation not yet supported')

            a = run(True)
            xs.append(Term('=', run(v), run(k)))
            return a

        else:
            v = VariableId()   # generate a new variable name.
            x = x.apply(run)

            if unif:
                xs.append(
                    BuildStructure(x.fn, arguments = x.args, result = v)
                )
            else:
                xs.append(
                    dyna_system.call_term(x.fn.getValue(Frame()), x.arity)(*x.args, ret = v)
                )

            return v

    if isinstance(x, Rule):
        # Note: we `run` in this order because it simulates Prolog's ordering.
        h,a,v,s = x
        h = run(h, unif=True)
        run(s)
        v = run(v)
        return Rule(h, a, v, xs, original=x)

    else:
        run(x)
        return xs


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


# TODO: these are currently just eye-ball tests. Need to make them pass/fail.
def test():

    x = normalize(term('f(X)'))
    print(x)

    x = normalize(term('f(X), g(X)'))
    print(x)

    [r] = run_parser('goal += f(X) * g(X).')
    x = normalize(r)
    print(x)


    # REFERENCE: Manual fib program.
    from dyna.builtins import gteq, lteq, sub, add
    from dyna import interpreter, M, Frame, constant, Unify, Partition, \
        variables_named, BuildStructure, MemoContainer, Intersect, \
        Terminal, Term, context, Aggregator, AggregatorOpImpl

    ret_variable = interpreter.ret_variable

    fib = Partition(variables_named(0, ret_variable),
                    (# fib(0) = 0
                     Intersect(Unify(constant(0), VariableId(0)), Unify(constant(0), ret_variable)),

                     # fib(1) = 1
                     Intersect(Unify(constant(1), VariableId(0)), Unify(constant(1), ret_variable)),

                     # fib(X) = X >= 2, X <= 150, fib(X-1) + fib(X-2).
                     Intersect(gteq(VariableId(0), constant(2)), lteq(VariableId(0), constant(150)),
                               sub(VariableId(0), constant(1), ret=VariableId('Xm1')),
                               sub(VariableId(0), constant(2), ret=VariableId('Xm2')),
                               dyna_system.call_term('fib', 1)(VariableId('Xm1'), ret=VariableId('F1')),
                               dyna_system.call_term('fib', 1)(VariableId('Xm2'), ret=VariableId('F2')),
                               add(VariableId('F1'), VariableId('F2'), ret=ret_variable)
                     )))
    dyna_system.define_term('fib', 1, fib)
    print(fib)


    for x in run_parser("""
    fib(0) = 1.
    fib(1) = 1.
    fib(N) = fib(N-1) + fib(N-2) for N > 1.
    """):
        print()
        print(colors.green % 'parsed:', x)
        print(colors.green % 'normed:', normalize(x))


if __name__ == '__main__':
    test()
