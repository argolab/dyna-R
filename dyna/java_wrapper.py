
def _configure_jvm():
    import os
    import jnius_config


    # this could call make which checks if the source files are up to date with the classes
    # I suppose that in the future we could compile a Jar instead of just having the class files in a directory
    d = os.path.join(os.path.dirname(__file__), '../dyna_backend_java/')
    build = os.popen(f'cd {d} && make all')
    res = build.read()
    if build.close() is not None:
        print("compile has failed")
        print(res)
        raise RuntimeError('compiling the scala backend has failed')

    # this is the current classes which represent which
    jnius_config.add_classpath(os.path.join(os.path.dirname(__file__), '../dyna_backend_java/target/scala-2.13/classes'))
    jnius_config.add_classpath('/usr/share/scala/lib/scala-library.jar')  # TODO: this should figure this out automattically

_configure_jvm()

from jnius import autoclass, JavaClass, MetaJavaClass, JavaStaticMethod, protocol_map
import jnius

# imo, jnius should really just include this stuff for object as all of the classes are going to have these methods
protocol_map.setdefault('java.lang.Object', {})
protocol_map['java.lang.Object']['__hash__'] = lambda self: self.hashCode()
protocol_map['java.lang.Object']['__eq__'] = lambda self, other: self.equals(other)
protocol_map['java.lang.Object']['__str__'] = lambda self: self.toString()

# this should be methods that are exposed on all terms
# if there are methods which we want to define for python methods
protocol_map['org.dyna.Term'] = {
    # this becomes a property on the resulting objects which means that we do need to overwrite this as having methods
    'name': property(lambda self: self.getName()),
    'get_arity': lambda self: self.getArity() ,  # I suppose that we can make this do the mapping to the underscore variables

    # if there are expressions which
}


Term = autoclass('org.dyna.term.Term')




import threading
_original_thread_class = threading.Thread

class ThreadMonkeyPatchClass:
    def __init__(self, group=None, target=None, name=None, args=(), kwargs={}, *, daemon=None):
        self.__target = target
        self.__args = args
        self.__kwargs = kwargs
        self.__thread = _original_thread_class(group=group, target=self.__our_run, name=name, args=(), kwargs={}, daemon=daemon)

    def __our_run(self):
        try:
            self.run()
        finally:
            # clean this thread from the jvm otherwise leaks
            jnius.detach()

    def run(self):
        self.__target(*self.__args, **self.__kwargs)

    def __getattr__(self, name):
        return getattr(self.__thread, name)

    def __setattr__(self, name, value):
        if type(self).__name__ in name or 'run' == name:
            object.__setattr__(self, name, value)
        else:
            setattr(self.__thread, name, value)

    # def start(self, *args, **kwargs): return self.__thread.start(*args, **kwargs)
    # def join(self, *args, **kwargs): return self.__thread.join(*args, **kwargs)

    # @property
    # def ident(self): return self.__thread.ident

    # @property
    # def native_id(self): return self.__thread.native_id

    # @property
    # def daemon(self): return self.__thread.daemon
    # @daemon.setter
    # def daemon(self, v): self.__thread.daemon = v
    # def isDaemon(self): return self.__thread.isDaemon()
    # def setDaemon(self, *args, **kwargs): return self.__thread.setDaemon(*args, **kwargs)

    # def is_alive(self): return self.__thread.is_alive()

    # @property
    # def name(self): return self.__thread.name
    # @name.setter
    # def name(self, v): self.__thread.name = v

    # def getName(self): return self.__thread.getName()
    # def setName(self, *args, **kwargs): return self.__thread.setName(*args, **kwargs)

    # def __str__(self)


threading.Thread = ThreadMonkeyPatchClass
