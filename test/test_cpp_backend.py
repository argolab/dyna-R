import pytest

#@pytest.mark.skip
def test_cpp_simple():
    import dyna_cpp_backend

    dyna_cpp_backend.terms.term_constructor("test", (123,))
