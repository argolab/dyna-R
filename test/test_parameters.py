from dyna import *

import pytest

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

    $parameters_next(&x) := x - gf * alpha.
    """)

    dyna.run_agenda()

    frame = Frame()
    r = simplify(dyna.call_term('x', 0), frame)
    assert r == Terminal(1)

    assert abs(interpreter.ret_variable.getValue(frame) - 1.0) < .001


def test_auto_diff1():
    dyna = context.SystemContext()

    dyna.add_rules("""
    $load("gradient").

    x := 0.

    f = (x - 1)^2.

    gx = $gradient(&x).

    $loss += f.
    """)
    dyna.run_agenda()

    frame = Frame()
    r = simplify(dyna.call_term('gx', 0), frame)
    assert r == Terminal(1)

    assert interpreter.ret_variable.getValue(frame) == 2.0


@pytest.mark.xfail
def test_auto_diff2():
    dyna = context.SystemContext()

    dyna.add_rules("""
    $load("gradient").

    x := 0.

    v(X) = exp(X).  % defined as a function which needs to have its gradient computed wrt its parameters

    f = (v(x + 1) - 1)^2.

    gx = $gradient(&x).

    $loss += f.
    """)
    dyna.run_agenda()

    frame = Frame()
    r = simplify(dyna.call_term('gx', 0), frame)
    assert r == Terminal(1)

    assert abs(interpreter.ret_variable.getValue(frame) - 9.34155) < .001
