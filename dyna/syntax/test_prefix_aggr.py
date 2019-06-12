from dyna.syntax.generic import Term, FVar, Rule
from dyna.syntax.aggregators import AGG
from dyna.syntax.syntax import term, run_parser
from dyna.syntax.normalizer import normalize, handle_disjunctions, gen_functor
from dyna.syntax.util import colors


def test_prefix_aggr():

    def test(x, y):
        gen_functor._i = 0   # reset gensym counter to get stable output
        Y = run_parser('\n'.join(y))
        [r] = run_parser(x, handle_disjunctions=False)
        #Z = run_parser(x, handle_disjunctions=True)

        print()
        print(colors.green % r)
        Z = handle_disjunctions(r)

        print(colors.green % 'input: ', x)
        print(colors.green % 'expect:')
        for y in Y:
            print(' ', y)
        print(colors.green % 'output:')
        for z in Z:
            print(' ', z)
        assert Y == Z, 'see above'

    test('goal = 5 * (max= f(X)).',
         ['goal = (5 * $gen1).',
          '$gen1 max= f(X).'])

    test('goal(X) = (+= f(X, Y, Z)) * g(Z).',
         ['goal(X) = ($gen1(X, Z) * g(Z)).',
          '$gen1(X, Z) += f(X, Y, Z).'])

    test('goal(X) = (+= f(X, Y, Z)) * (+= g(Z)).',
         ['goal(X) = ($gen1(X, Z) * $gen2(Z)).',
          '$gen1(X, Z) += f(X, Y, Z).',
          '$gen2(Z) += g(Z).'])

    test('goal(X) = (+= f(X, Y) * (+= g(Y, Z))).',
         ['goal(X) = $gen2(X).',
          '$gen1(Y) += g(Y, Z).',
          '$gen2(X) += (f(X, Y) * $gen1(Y)).',
          ])

    test('goal = (+= f(X)) * (+= g(X)).',
         ['goal = ($gen1(X) * $gen2(X)).',
          '$gen1(X) += f(X).',
          '$gen2(X) += g(X).'])

    test('goal(X) += h(X) * sigmoid(?= f(X,Y) * (+= g(Y, Z))).',
         ['goal(X) += (h(X) * sigmoid($gen2(X))).',
          '$gen1(Y) += g(Y, Z).',
          '$gen2(X) ?= (f(X, Y) * $gen1(Y)).'])

    # TODO: check against the design on the issue tracker
    # https://github.com/nwf/dyna/issues/43
    test("goal(*) = 5 for X=Y.",
         ['goal(&$gen1(X,Y)) = 5 for X=Y.'])

    # Check that gensyms are allocated after prefix aggregator transform.
    test("edge(A,*) max= g(A) / (+= g(A,B)).",
         ['edge(A, &$gen2(A)) max= g(A) / $gen1(A).',
          '$gen1(A) += g(A, B).'
         ])

    # Test "cylindrification" of the disjuncts
    test("goal(Y) max= h(X) for (?f(X) ; ?g(Y, Z) ; 5*f(X) > 1).",
         ['goal(Y) max= h(X) for $gen1(X, Y).',
          '$gen1(X, Y) :- ?f(X).',
          '$gen1(X, Y) :- ?g(Y, Z).',
          '$gen1(X, Y) :- 5*f(X) > 1.',
         ])

    # Test multiple disjuncts with a prefix aggregator
    test("goal(Y) max= h(X) for (min= f(X) ; g(Y, Z) ; h(X)) > 5.",
         ['goal(Y) max= h(X) for $gen1(X, Y) > 5.',
          '$gen1(X,Y) min= f(X).',
          '$gen1(X,Y) min= g(Y, Z).',
          '$gen1(X,Y) min= h(X).',
         ])

    # Test sugar for unless
    test("goal(Y) max= h(X) unless f(X,Y,Z).",
         ['goal(Y) max= h(X) for !$gen1(X,Y).',
          '$gen1(X,Y) |= false.',
          '$gen1(X,Y) |= f(X,Y,Z).',
         ])


if __name__ == '__main__':
    test_prefix_aggr()
