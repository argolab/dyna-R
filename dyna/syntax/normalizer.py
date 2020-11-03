from copy import deepcopy
import textwrap

from dyna.syntax.generic import Term, FVar, Rule
from dyna.syntax.aggregators import AGG
from dyna.syntax.syntax import term, run_parser
from dyna.syntax.util import colors, fib_check

from dyna.interpreter import VariableId, constant, ConstantVariable, intersect, InvalidValue, FinalState
from dyna.optimize import run_optimizer
#from dyna.context import dyna_system

from dyna import Frame, Terminal, Aggregator, Unify, interpreter, \
    Partition, loop, saturate, AggregatorOpImpl, \
    BuildStructure
from dyna.terms import CallTerm


DEBUG = False


def normalize(x, dyna_system):
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


from dyna.aggregators import AGGREGATORS as AGGR, colon_line_tracking
# AGGR = {
#     '=': AggregatorOpImpl(lambda a,b: 1/0),   # should never combine
#     '+=': AggregatorOpImpl(lambda a,b: a+b),
#     '*=': AggregatorOpImpl(lambda a,b: a*b),
#     'max=': AggregatorOpImpl(max),
#     'min=': AggregatorOpImpl(min),
#     ':-': AggregatorOpImpl(lambda a,b: a or b),  # TODO:...  this should terminate early in the case that this identifies that there is a true value.
#     '|=': AggregatorOpImpl(lambda a,b: a or b),
#     '&=': AggregatorOpImpl(lambda a,b: a and b)
# }
#colon_line_tracking = 0


def add_rule(x, dyna_system=None):
    if dyna_system is None:
        # use a global references if not defined
        from dyna import dyna_system

    if DEBUG: print()
    if DEBUG: print(colors.green % 'parsed:', x)

    # The "direct" translation of Dyna into R-exprs will create named calls
    # for each distinct functor/arity

    r = normalize(x, dyna_system)

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


    if r.aggr == ':-':
        body = intersect(Unify(iret, constant(True)), body)
    elif r.aggr == ':=':
        #global colon_line_tracking
        niret = VariableId()
        body = intersect(body, BuildStructure('$colon_line_tracking', niret,
                                              (constant(colon_line_tracking()), iret)))
        iret = niret
        #colon_line_tracking += 1


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


def add_rules(rules, system=None):
    for x in run_parser(rules):
        add_rule(x, system)


def user_query_to_rexpr(x, dyna_system=None):
    "Map a user's textual query to an R-expression."
    if dyna_system is None:
        # use a global references if not defined
        from dyna import dyna_system

    q = run_parser(f'{x} ?')
    if DEBUG: print(colors.green % 'parsed:', q)
    q = normalize(q, dyna_system)
    if DEBUG: print(colors.green % 'normed:', q)
    q = intersect(*q)
    if DEBUG: print(colors.green % 'rexped:', q)
    if DEBUG: print(colors.green % 'vardom:', list(q.all_vars()))
    return q


def user_query(x):
    print(colors.light.yellow % 'user query:', x)
    r = user_query_to_rexpr(f'Result is ({x})')

    # extracts the user's variable names
    user_vars = list(dict((v, 0) for v in r.all_vars()
                 if not str(v).startswith('$') and not isinstance(v, ConstantVariable)
    ).keys())

    results = []
    def callback(rr, ff):
        values = []
        for v in user_vars:
            r = v.getValue(ff)
            if r is InvalidValue:
                values.append(f"'{v}': ---")
            else:
                # try to print this as a list
                try:
                    rl = r.aslist()
                    if rl is not None:
                        r = rl
                except:
                    pass
                values.append(f"'{v}': {r}")

        # bind variables to their value in the frame for printing
        if isinstance(rr, FinalState):
            rbf = rr
        else:
            rbf = rr.rename_vars(lambda v: constant(v.getValue(ff)) if v.isBound(ff) else v)
            rbf, _ = run_optimizer(rbf, user_vars)

        values = colors.yellow % 'result: ' + '{'+', '.join(values)+'} ' + colors.yellow % '@'
        rexpr = textwrap.indent(str(rbf), ' '*(len(values) - 2*len(colors.yellow)+5)).strip()
        print(values, rexpr)

        # print(colors.yellow % 'result:', '{'+', '.join(values)+'}',
        #       colors.yellow % '@', rr)

        results.append([rr, deepcopy(ff)])

    frame = Frame()

    #print(r)
    ro, _ = run_optimizer(r, user_vars)
    rr = saturate(ro, frame)
    # this could run the optimizer and rerun saturate on the user's query?
    # though that does not necessarily result in something better?

    #print(rr)
    loop(rr,
         frame, callback,
         best_effort=True
         )

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
