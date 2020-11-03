import os
from dyna.api import DynaAPI
import pytest


@pytest.mark.xfail(reason="some missmatch between the syntax from dyna-pi and dyna-R")
def program_gibbs(d):
    d.add_rules("""
    % messages from potentials given the current variable assignment
    m(X, I) *= phi(I, J, X, x(J)).

    % distribution is proportional to the aggregate message.
    p(X, I) = m(X, I) / (+= m(_, I)).

    % define a simple graph
    nodes(1).
    nodes(2).
    nodes(3).
    nodes(4).
    domain("R",_).
    domain("G",_).
    domain("B",_).

    phi(I, J, X, Y) = ((X == Y) + 0.1)
       for I: nodes, J: nodes,
           X: domain(I), Y: domain(J).

    x(1) := "B".
    x(2) := "R".
    x(2) := "B".
    x(3) := "R".
    """)

    assert d.make_call('x(%)').to_dict() == {(1,): 'B', (2,): 'B', (3,): 'R'}

    assert d.make_call('p(%, 1)').to_dict() == {
        # TODO: fill in the dictonary that is returned
    }

    assert False

def program_quicksort(d):
    d.add_rules("""

    partition([], _, [], []).
    partition([X|Xs], Pivot, [X|S], B) :-
        partition(Xs, Pivot, S, B) for X < Pivot.
    partition([X|Xs], Pivot, S, [X|B]) :-
        partition(Xs, Pivot, S, B) for X >= Pivot.

    quicksort([]) := [].
    quicksort([X|Xs]) :=
        partition(Xs, X, S, B),
        concat(quicksort(S), [X | quicksort(B)]).

    concat([], Y) := Y.
    concat(X, []) := X.
    concat([X|Xs], Y) := [X|concat(Xs, Y)].

    """)

    assert d.call('quicksort([2,3,4,1])') == [1,2,3,4]


def program_transformation_learning(d):
    return
    d.add_rules(open(os.path.dirname(__file__) + '../examples/transformation_learning.dyna').read())
    d.run_agenda()

def program_edit_distance(d):
    d.add_rules("""

    % Base case: distance between two empty strings.
    dist([],[]) min= 0.

    % Recursive cases.
%    dist([X|Xs]   , Ys) min= del(X)   + dist(Xs,Ys).
%    dist(Xs,    [Y|Ys]) min= ins(Y)   + dist(Xs,Ys).
%    dist([X|Xs],[Y|Ys]) min= sub(X,Y) + dist(Xs,Ys).

    dist([X|Xs],     Ys) min=   del + dist(Xs,Ys).
    dist(    Xs, [Y|Ys]) min=   ins + dist(Xs,Ys).
    dist([X|Xs], [Y|Ys]) min=     0 + dist(Xs,Ys) for X==Y.
    dist([X|Xs], [Y|Ys]) min= subst + dist(Xs,Ys) for X!=Y.

    del := 1.
    ins := 1.

    % Part of the cost function.
%    sub(L1,L2) := subst for L1 =/= L2.
%    sub( L, L) := 0.  % cost of 0 to align any letter to itself

%    sub(_, _) := subst.
%    sub(L, L) := 0.     % cost of 0 to align any letter to itself

%    ins(_) := 1.
%    del(_) := 1.

    """)

    d.add_rules('subst := 1.')
    assert d.call('dist(["a","b","c","d"], ["s","b","c","t","d"])') == 2

    # TODO: get this query to work -- need to define an alphabet.
    #assert_equal_query(d, 'dist(["a","a"], [X:alphabet,Y:alphabet]) <= 1', 1)

    # TODO: get this to work (the issue is that we have an intermediate
    # nonground update; we have the machinery to backoff to invalidations, but
    # it's not being triggered for some reason).
    d.add_rules('subst := 1.5.')
    assert d.call('dist(["a","b","c","d"], ["s","b","c","t","d"])') ==  2.5


def program_value_iteration(d):
    d.add_rules("""
    γ := 0.5.

    % The optimal value function V .
    v(S) max= q(S,A).

    % The optimal action-value function Q.
    % Note: The value of p(s, a, s') is a conditional transition probability, P(s' | s, a).
    q(S,A) += r(S,A).
    q(S,A) += γ * p(S,A,Sp) * v(Sp).

    % The optimal policy function π. The free-choice aggregator ?= is used
    % merely to break ties as in footnote 17.
    pi(S) ?= A for v(S) == q(S,A).

    r("a", 0) := 0.
    r("a", 1) := 1.
    p("a", 0, "a") := 1.
    p("a", 1, "a") := 1.
    """)

    d.make_call('v/1').set_memoized('null')
    d.make_call('pi/1').set_memoized('null')

    assert d.make_call('v(%)').to_dict() == {("a",): 2.0}
    assert d.make_call('pi(%)').to_dict() == {("a",): 1.0}


def program_value_iteration_prefix_aggr(d):
    d.add_rules("""
    γ := 0.5.

    % The optimal value function V .
    q(S, A) += r(S,A).
    q(S, A) += γ * p(S,A,Sp) * (max= q(Sp, Ap)).

    % The optimal policy function π. The free-choice aggregator ?= is used
    % merely to break ties as in footnote 17.
    pi(S) ?= A for q(S,A) == (max= q(S,Ap)).

    r("a", 0) := 0.
    r("a", 1) := 1.
    p("a", 0, "a") := 1.
    p("a", 1, "a") := 1.
    """)

    d.make_call('q/2').set_memoized('null')
    d.make_call('pi/1').set_memoized('null')

    assert d.make_call('q(%, %)').to_dict() == {("a", 0): 1.0, ("a", 1): 2.0}
    assert d.make_call('pi(%)').to_dict() == {("a",): 1.0}


def program_markov_reward_process(d):
    d.add_rules("""
    discount := 0.5.

    value(S) += reward(S).
    value(S) += discount * p(S, Sp) * value(Sp).

    reward("a") := 1.
    p("a", "a") := 1.
    """)

    d.make_call('value/1').set_memoized('null')  # this is going ot have to make the values table memoized?  Should work with either unk or null I think

    assert d.make_call('value(%)').to_dict() == {("a",): 2.0}


def program_linear_regression1(d):
    d.add_rules("""

    slope := covXY/varX.
    intercept := avgY - slope * avgX.

    avgY += y(I)/n.
    avgX += x(I)/n.

    varX += (x(I) - avgX)^2 / n.
    varY += (y(I) - avgY)^2 / n.

    covXY += (x(I) - avgX) * (y(I) - avgY) / n.
    %n += 1 for ?x(I), ?y(I).  % this line is wrong.  the ?foo(X) can not be used to iterate the domain of X
    n += 1 for _ is x(I), _ is y(I).

    % Synthetic `y` variable: a noisy linear function of `x`.
    y(I) := a*x(I) + b.
    """)

    d.add_rules("""
    x(0) := 1.
    x(1) := 2.
    x(2) := 3.
    x(3) := 4.
    """)

    d.add_rules('a := -3.')
    d.add_rules('b := 2.')

    assert d.call('n') == 4
    assert d.call('slope') == -3
    assert d.call('intercept') == 2

    d.add_rules('a := 4.')
    d.add_rules('b := -1.')

    assert d.call('slope') == 4
    assert d.call('intercept') == -1

    d.add_rules('x(4) := 5.')

    assert d.call('n') == 5
    assert d.call('slope') == 4
    assert d.call('intercept') == -1



def _make_test_cases():
    def make_func(method):
        def f():
            d = DynaAPI()
            method(d)
        if hasattr(method, 'pytestmark'):
            f.pytestmark = method.pytestmark
        return f
    for key, val in list(globals().items()):
        if key.startswith('program_'):
            globals()['test_' + key] = make_func(val)
_make_test_cases()
