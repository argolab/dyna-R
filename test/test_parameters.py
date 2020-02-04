from dyna import *


def test_basic_sgd():
    dyna = context.SystemContext()
    #dyna.run_agenda()

    # define a single convex function and then optimize it with gradient decent
    dyna.add_rules("""
    $load("parameters").

    x := 0.
    x := $parameters(&x).

    f = (x - 1)^2.   % the function f
    gf = 2*(x - 1).  % the gradient

    alpha = 0.05.

    $parameters_next(&x) := x + gf * alpha.
    """)

    # when running the agenda, this should identify that
    #import ipdb; ipdb.set_trace()
    dyna.run_agenda()

    frame = Frame()
    r = simplify(dyna.call_term('x', 0), frame)
    assert r == Terminal(1)

    assert interpreter.ret_variable.getValue(frame) == 1.0
