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
