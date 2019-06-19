from copy import deepcopy

from dyna.syntax.generic import Term, FVar, Rule
from dyna.syntax.aggregators import AGG
from dyna.syntax.syntax import term, run_parser
from dyna.syntax.util import colors, fib_check

from dyna.interpreter import VariableId, constant, ConstantVariable, intersect
from dyna.context import dyna_system

from dyna import Frame, Terminal, Aggregator, Unify, interpreter, \
    Partition, loop, saturate, AggregatorOpImpl, \
    BuildStructure
from dyna.terms import CallTerm


DEBUG = False


def normalize(x):
    """
    Normalize `x` into a form where all intermediate queries have a value slot.
    """

    xs = []
    n = 0

    def genvar():
        return VariableId()
        # nonlocal n
        # n += 1
        # return VariableId(f'$V{n}')

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


AGGR = {
    '=': AggregatorOpImpl(lambda a,b: 1/0),   # should never combine
    '+=': AggregatorOpImpl(lambda a,b: a+b),
    'max=': AggregatorOpImpl(max),
    'min=': AggregatorOpImpl(min),
}


def add_rule(x):
    if DEBUG: print()
    if DEBUG: print(colors.green % 'parsed:', x)

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
    body = intersect(
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

    if DEBUG: print(colors.green % 'rule', rule)
    #rule = run_optimizer(rule, (*args, interpreter.ret_variable))
    #if DEBUG: print(colors.light.yellow % 'optimized rule', rule)

    dyna_system.add_to_term(head.name, arity, rule)


def add_rules(rules):
    for x in run_parser(rules):
        add_rule(x)


def user_query_to_rexpr(x):
    "Map a user's textual query to an R-expression."
    q = run_parser(f'{x} ?')
    if DEBUG: print(colors.green % 'parsed:', q)
    q = normalize(q)
    if DEBUG: print(colors.green % 'normed:', q)
    q = intersect(*q)
    if DEBUG: print(colors.green % 'rexped:', q)
    if DEBUG: print(colors.green % 'vardom:', list(q.all_vars()))
    return q


def user_query(x):
    print(colors.light.yellow % 'user query:', x)
    r = user_query_to_rexpr(f'Result is ({x})')

    # extracts the user's variable names
    user_vars = [v for v in r.all_vars()
                 if not str(v).startswith('$') and not isinstance(v, ConstantVariable)
    ]

    results = []
    def callback(rr, ff):
        print(colors.yellow % 'result:', {str(v): v.getValue(ff) for v in user_vars},
              colors.yellow % '@', rr)

        results.append(deepcopy([rr, ff]))

    frame = Frame()

    #print(r)
    rr = saturate(r, frame)
    #print(rr)
    loop(rr,
         frame, callback, best_effort=True)

    print()

    return results


def test_fib():

    for x in run_parser("""
    fib(0) = 1.
    fib(1) = 1.
    fib(N) = fib(N-1) + fib(N-2) for N > 1, N <= 10.
    """):
        add_rule(x)

#    def run_fib(N):
#        fib_call = dyna_system.call_term('fib', 1)
#        frame = Frame()
#        rr1 = saturate(fib_call, frame)
#        frame[0] = N
#        rr = saturate(rr1, frame)
#        assert rr == Terminal(1), [rr, rr1]
#        #if DEBUG: print(frame)
#        return interpreter.ret_variable.getValue(frame)

    user_query('fib(5)')
    user_query('fib(N) for range(N,0,11)')
#    user_query('fib(N)')

    # TODO: use `Result` classes from dyna-pi for displaying results.

    # TODO: this doesn't work for some reason...
#    print('------')
#    user_query('N^2+1/N for range(N,0,11)')


def test_mapl_neural_network():
    add_rules("""
    weights(0)  = 2.
    weights(-1) = 3.
    weights(1)  = 5.

    neural_input(&inp(X)) = weights(X).
    neural_output(X) += neural_edge(X, Y) * neural_input(Y).
    neural_edge(&out(X+Z), &inp(X)) = weights(Z).
    """)

    user_query('neural_output(&out(0))')
    user_query('neural_output(X)')


def test_simple():

    for x in run_parser("""
    goal += 1.
    goal += 2.
    """):
        add_rule(x)

    user_query('goal')


if __name__ == '__main__':
    test_simple()
    test_fib()
    test_mapl_neural_network()