from dyna import *

def test_most_basic():
    # X+Y
    rexpr, ret = M.add(1,2)

    frame = interpreter.Frame()
    frame[1] = 7
    frame[2] = 8
    rr = interpreter.simplify(rexpr, frame)

    assert rr == interpreter.Terminal(1)
    assert ret.getValue(frame) == 15


def test_two_add():
    # (X + Y) + Z

    rexpr1, ret1 = M.add(1,2)
    rexpr2, ret2 = M.add(ret1, 3)

    rexpr = Intersect(rexpr1, rexpr2)

    frame = Frame()
    frame[1] = 7
    frame[2] = 8
    frame[3] = 9
    rr = simplify(rexpr, frame)

    assert rr == interpreter.Terminal(1)
    assert ret2.getValue(frame) == 24


def test_simple_range():
    rexpr, ret = M.range(1,2,3)

    r = {ret: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    frame[1] = 4  # 1 < 4 < 7
    frame[2] = 1
    frame[3] = 7

    rr = simplify(rexpr, frame)
    assert rr == Terminal(1)


def test_sum_aggregator():
    # f(X, Y) += R for R:X..Y.
    v1,v2,v3,v4 = variables_named(1,2,3,4)

    rexpr, ret1 = M.range(v1,v2,v3)
    rexpr = Aggregator(v4, (v2,v3), v1, AggregatorOpImpl(lambda a,b: a+b), rexpr)

    r = {ret1: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    v2.setValue(frame, 1)
    v3.setValue(frame, 4)

    rr = simplify(rexpr, frame)

    assert rr == Terminal(1)
    assert v4.getValue(frame) == 6
