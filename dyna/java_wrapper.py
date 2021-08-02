import jpype as _jpype

def _configure_jvm():
    import os

    d = os.path.join(os.path.dirname(__file__), '../dyna_backend_java2')
    build = os.popen(f'cd {d} && make all')
    res = build.read()
    if build.close() is not None:
        print("compile has failed")
        print(res)
        raise RuntimeError('compiling the backend has failed')

    # this is the current classes which represent which
    _jpype.addClassPath(os.path.join(d, 'target/dyna_backend-0.1.0-SNAPSHOT-standalone.jar'))

_configure_jvm()

_jpype.startJVM()

_jpype.JClass('java.lang.Object').__repr__ = lambda self: f'Backend<{str(self)}>'

_interface = _jpype.JClass('dyna_backend.DynaInterface')()


def _construct_make_method(name):
    def f(*args):
        return _interface.make_rexpr(name, args)
    f.__name__ = f'make_{name}'
    return f

make_variable = _interface.make_variable
make_constant = _interface.make_constant

for _name in {'unify', 'conjunct', 'disjunct', 'multiplicity', 'proj', 'aggregator', 'if'}:
    globals()[f'make_{_name}'] = _construct_make_method(_name)
