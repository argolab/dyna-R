from dyna.api import DynaAPI

import pytest

def test_python_api():
    api = DynaAPI()

    @api.define_function()
    def my_function(arg1):
        return arg1 * 7

    api.add_rules("""
    fib(X) = fib(X-1) + fib(X-2) for X > 1.
    fib(1) = 1.
    fib(0) = 0.

    table(0) = 1.
    table(1) = 2.
    table(2) = 3.
    table(3) = 4.

    two_c(0, 1) = 2.
    two_c(0, 2) = 3.
    two_c(1, 3) = 4.
    two_c(1, 4) = 5.
    """)

    fib = api.make_call('fib/1')
    assert fib(10) == 55

    table = api.make_call('table(%)')
    assert table(1) == 2
    assert table[2] == 3


    vals = {key: val for ((key,), val) in table}
    assert vals == {0:1, 1:2, 2:3, 3:4}

    two_c = api.make_call('two_c(%,%)')
    assert {k: v for ((k,), v) in two_c[0, :]} == {1:2, 2:3}
    assert {k: v for ((k,), v) in two_c[1, :]} == {3:4, 4:5}

    assert {k: v for ((k,), v) in api.make_call('two_c(0, %)')} == {1:2, 2:3}
    assert {k: v for ((k,), v) in api.make_call('two_c(1, %)')} == {3:4, 4:5}

    assert {k: v for ((k,), v) in api.make_call('two_c(%, 3)')} == {1: 4}

    return


    table2 = api.table('table2', 1)
    table2[7] = 123
    table2[8] = {'python opaque dict': 999}  # any value that we do not know how to deal with will essentially be opaque, but is still passed around

    api.add_rules("""
    cnt_table2 += 1 for X is table2(Y).
    """)

    assert api.call('cnt_table2') == 2
