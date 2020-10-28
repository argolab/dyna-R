from dyna import *
from dyna.trace_gradient import *
import pytest

@pytest.mark.xfail()
def test_trace_simple():
    value = TracedValue(1)

    res = value * 2

    run_backwards(res)
