import jpype

def _configure_jvm():
    import os

    d = os.path.join(os.path.dirname(__file__), '../dyna_backend_java/')
    build = os.popen(f'cd {d} && make all')
    res = build.read()
    if build.close() is not None:
        print("compile has failed")
        print(res)
        raise RuntimeError('compiling the scala backend has failed')

    # this is the current classes which represent which
    jpype.addClassPath(os.path.join(os.path.dirname(__file__), '../dyna_backend_java/target/scala-2.13/classes'))

_configure_jvm()

jpype.startJVM()

Term = jpype.JClass('org.dyna.Term')
