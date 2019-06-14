from dyna.syntax.generic import Term, FVar, Rule
from dyna.syntax.aggregators import AGG
from dyna.syntax.syntax import term, run_parser

from dyna.interpreter import VariableId, constant
from dyna import BuildStructure
from dyna.context import dyna_system
from dyna.syntax.util import colors

from dyna import Frame, Terminal
from dyna import Aggregator, Unify, interpreter, Partition
from dyna import saturate, AggregatorOpImpl
from dyna.terms import CallTerm


def normalize(x):
    """
    Normalize `x` into a form where all intermediate queries have a value slot.
    """

    xs = []
    n = 0

    def genvar():
        nonlocal n
        n += 1
        return VariableId(f'$V{n}')

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
            xs.append(Unify(run(v), run(k)))

            return a

        else:
            v = genvar()   # generate a new variable name.
            x = x.apply(run)

            fn = x.fn.getValue(Frame()) if not isinstance(x.fn, str) else x.fn
            if unif:
                xs.append(
                    BuildStructure(fn, arguments = x.args, result = v)
                )
            else:
                xs.append(
                    dyna_system.call_term(fn, x.arity)(*x.args, ret = v)
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


AGGR = {
    '=': AggregatorOpImpl(lambda a,b: 1/0),   # should never combine
    '+=': AggregatorOpImpl(lambda a,b: a+b),
    'max=': AggregatorOpImpl(max),
    'min=': AggregatorOpImpl(min),
}


def add_rule(x):
    print()
    print(colors.green % 'parsed:', x)

    # The "direct" translation of Dyna into R-exprs will create named calls
    # for each distinct functor/arity

    r = normalize(x)

    # patch-in my return variable with the expected return variable
    head = None
    for s in r.sides:
        # grab build structure operation that corresponds to the head of the rule
        if isinstance(s, BuildStructure) and s.result == r.head:
            head = s
    assert head is not None
    r.sides.remove(head)


    arity = len(head.arguments)
    args = [VariableId(i) for i in range(arity)]

    # Move commas to the front of the subgoal list...
    # TODO: the optimizer should be able to take care of this.
    commas = []
    for s in r.sides:
        if isinstance(s, CallTerm) and s.term_ref == (',', 2):
            commas.append(s)
    for s in commas:
        r.sides.remove(s)
    r.sides = commas + r.sides
    iret = VariableId()
    body = interpreter.intersect(
        *[Unify(VariableId(i), a) for i, a in enumerate(head.arguments)],
        *r.sides,
        Unify(iret, r.value),
    )

    # rename the variables that are not explicitly referenced to be unique to this rule
    rm = {VariableId(x): VariableId(x) for x in range(arity)}
    rm[iret] = iret
    body = body.rename_vars_unique(rm.get)


    rule = Aggregator(interpreter.ret_variable,
                      args,
                      iret,         # inner return; value being aggregated
                      AGGR[r.aggr],
                      Partition((*args, iret), [body]))

    assert iret in set(body.all_vars())

    #from dyna.optimize import run_optimizer
    print(colors.green % 'rule', rule)
    #rule = run_optimizer(rule, (*args, interpreter.ret_variable))
    #print(colors.light.yellow % 'optimized rule', rule)

    dyna_system.add_to_term(head.name, arity, rule)


DEBUG = True

def user_query_to_rexpr(x):
    "Map a user's textual query to an R-expression."
    q = run_parser(f'{x} ?')
    if DEBUG: print(colors.green % 'parsed:', q)
    q = normalize(q)
    if DEBUG: print(colors.green % 'normed:', q)
    q = interpreter.intersect(*q)
    if DEBUG: print(colors.green % 'rexped:', q)
    if DEBUG: print(colors.green % 'vardom:', q.vars)
    return q


def user_query(x):
    r = user_query_to_rexpr(x)

    def callback(rr, ff):
        print(colors.yellow % 'result:', {v: v.getValue(ff) for v in r.vars},
              colors.yellow % '@', rr)

        #print('  value:', r.var_map[interpreter.ret_variable].getValue(ff))
        #print('  value', interpreter.ret_variable.getValue(ff))
        #from IPython import embed; embed()

    frame = Frame()
    interpreter.loop(saturate(r, frame), frame, callback)
    #interpreter.loop(r, frame, callback, till_terminal=True)


def test():

    for x in run_parser("""
    fib(0) = 1.
    fib(1) = 1.
    fib(N) = fib(N-1) + fib(N-2) for N > 1, N <= 10.
    """):
        add_rule(x)

    def run_fib(N):
        fib_call = dyna_system.call_term('fib', 1)
        frame = Frame(); frame[0] = N     # $0 = 0
        rr = saturate(fib_call, frame)
        assert rr == Terminal(1)
        #print(frame)
        return interpreter.ret_variable.getValue(frame)

    f = {0: 1, 1: 1, 2: 2, 3: 5, 4: 8, 5: 12, 6: 5+12}
    for N in range(0, 6):
        print(f'fib({N})')
        got = run_fib(N)         # TODO: use new user_query method.
        print('  =', got)
        assert got == f[N], f'`fib({N})` got `{got}`, expected `{f[N]}`.'
        print()

    print('------')

    user_query('fib(5)')


    # TODO: create use answer type and printing from dyna-pi


if __name__ == '__main__':
    test()
