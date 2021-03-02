import pytest


def test_java_simple():

    import dyna.java_wrapper

    # print('dict:' , dyna.java_wrapper.Term_auto.constructTerm.__dict__)
    # import ipdb; ipdb.set_trace()

    res = dyna.java_wrapper.Term.constructTerm('test', [])

    # assert res.foo() == 123

    # assert res.name_prop == 'test'
