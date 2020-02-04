from dyna import *


def test_basic_sgd():
    dyna = context.SystemContext()

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
    dyna_system.run_agenda()
