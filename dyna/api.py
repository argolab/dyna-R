# a public api for dyna to make working with the system a bit easier hopefully

import re
import inspect

from dyna import context
from dyna.interpreter import simplify, Frame, Terminal, ret_variable, UnificationFailure as DynaUnificationFailure
from dyna.builtins import moded_op
from dyna.syntax.normalizer import user_query_to_rexpr, run_parser


class WrappedOpaqueObject:
    # make it so that the value has a __hash__ and __eq__ method so that we can insert it into the trie etc
    # though with something like a dict, this is going to not be based on the actual value but rather the pointer identity...
    __slots__ = ('_value')
    def __init__(self, value): self._value = value
    def __hash__(self): return id(self._value)
    def __eq__(self, other):
        if not isinstance(other, WrappedOpaqueObject):
            return False
        return self._value is other._value
    def __str__(self): return str(self._value)
    def __repr__(self): return repr(self._value)

def cast_to_dyna(value):
    if isinstance(value, list):
        return Term.fromlist([cast_to_dyna(v) for v in value])
    if value.__hash__ is None:
        # we require that everything can be hashed (so that we can insert it into the prefix tries if required
        return WrappedOpaqueObject(value)
    return value

def cast_from_dyna(value):
    if isinstance(value, Term):
        try:
            return [cast_from_dyna(v) for v in value.aslist()]
        except (TypeError,AttributeError):
            pass
    if isinstance(value, WrappedOpaqueObject):
        return value._value
    return value


def construct_call(system, string):
    m = re.match(r'([a-z][a-zA-Z0-9_\$]*)/([0-9]+)', string)
    if m:
        name = m.group(1)
        arity = int(m.group(2))
        return system.call_term(name, arity), arity
    var_idx = -1
    def var_name(match):
        nonlocal var_idx
        var_idx += 1
        return f'ARGUMENT_{var_idx}'

    # support an expression like foo(%, %, 123)
    s = re.sub(r'%', var_name, string)
    var_idx += 1
    if 'Result' not in s:  # the user could write 'Result is foo(%, Y), Y is something(%, 123)'
        s = f'Result is {s}'
    rep = {
        VariableId('Result'): ret_variable,
    }
    for i in range(var_idx):
        rep[VariableId('ARGUMENT_{i}')] = VariableId(i)
    rexpr = user_query_to_rexpr(s, dyna_system=system)
    rexpr = rexpr.rename_vars_unique(rep.get)
    return rexpr, var_idx


class DynaIncompleteComputationException(Exception):

    def __init__(self, rexpr):
        super().__init__('Computation was incomplete, use `api.expose_rexprs = True` to see the internal result')
        self.rexpr = rexpr


class DynaIterableExpression:

    def __init__(self, system, rexpr, arity):
        self._system = system
        self._rexpr = rexpr
        self._arity = arity

    def __getitem__(self, key):
        if not isinstance(key, tuple):
            key = key,
        assert len(key) == self._arity
        if slice(None) in key:
            # then there are still free variables which are in this expression, so this is going to return a new item
            pass
        raise NotImplemented()

    def __iter__(self):
        return callback_to_iterator(self.callback)

    def callback(self, cb):
        # this is what the internal system is doing instead, so this is
        raise NotImplemented()

class DynaExpressionWrapper:
    # this will wrap a single method or statement in the program.  So that it can

    def __init__(self, api, statement):
        self._api = api
        self._statement = statement
        self._call = self._system.call_term(name, arity)  # if there are values which are expressed here, then it should

    def __call__(self, *args):
        self._api._check_run_agenda()
        frame = Frame({i: a for i,a in enumerate(args)})
        r = simplify(self._call, frame)
        if r == Terminal(0):
            return None
        if r == Terminal(1):
            return ret_variable.getValue(frame)
        if self._api._expose_rexprs:
            return r
        else:
            raise DynaIncompleteComputationException(r)

    def __getitem__(self, args):
        return self.__call__(*args)  # I think that this is basically going to look the same for us in dyna

    def __setitem__(self, key, value):
        # this should delcare a new rule into the program.  It will want to cause that
        # if this just defines an expression using :=, then it would
        pass

    def set_memoized(self, mode):
        assert mode in ('null', 'unk', 'off')
        pass


class DynaAPI:

    def __init__(self, program=None):
        self._system = context.SystemContext()

        # the auto run agenda should call the agenda method before it makes any queries into the program
        self._auto_run_agenda = True
        self._expose_rexprs = False

        if program:
            self.add_rules(program)

    def _check_run_agenda(self):
        if self._auto_run_agenda:
            self.run_agenda()

    def call(self, method, *args):
        # use this like foo/2 or foo(%,%) where it would identify that the %
        # symbols should be replaced with placeholder variables for some
        # expression
        c = self.make_call(method)
        return c(*args)


    def make_call(self, method):
        # if it matches the expression where it would have some expression that corresponds with
        return ExpressionWrapper(self._system, method)

    def table(self, name, arity):
        pass

    def define_function(self, name=None, arity=None):
        """
        This could be used as:

        @api.define_function()
        def my_function(arg1, arg2):
            return arg1+arg2

        then my_function/2 would be exposed in the dyna runtime for the all ground mode.  This could be done just using the builtins to expose these methods
        simply.
        """
        def f(func):
            lname, larity = name, arity
            if lname is None:
                lname = func.__name__
            if larity is None:
                larity = len(inspect.getargspec(func).args)
            assert larity > 0, "function should have arguments, otherwise it is just a constant"
            wf = lambda x, *args: (cast_to_dyna(func(*(cast_from_dyna(a) for a in args))), *args)
            d = {
                (False,)+((True,)*larity): wf
            }
            r = moded_op(lname, d)
            self._system.define_term(lname, larity, r)
            return func
        return f

    def run_optimizer(self):
        self._system.optimize_system()

    def run_agenda(self):
        self._system.run_agenda()

    def add_rules(self, rules):
        self._system.add_rules(rules)


def callback_to_iterator(func):
    # due to the technical differences between iterators and callbacks, we need this nasty method
    # only one of these two threads is ever running at a time, so not a big issue...
    import threading
    send = threading.Semaphore(0)
    recv = threading.Semaphore(0)
    done = False
    value = None
    def callback(v):
        nonlocal value, done, send, recv
        value = v
        recv.release()
        send.acquire()
        if done:
            raise KeyboardInterrupt()
        return value
    def threadF():
        nonlocal value, done, send, recv
        send.acquire()
        try:
            if not done:
                func(callback)
        except KeyboardInterrupt:
            pass
        finally:
            done = True
            recv.release()

    def iterator():
        nonlocal value, done, send, recv
        try:
            while True:
                send.release()
                recv.acquire()
                if done: break
                value = (yield value)
        finally:
            done = True
            send.release()
    thread = threading.Thread(target=threadF, daemon=True)
    thread.start()

    return iterator()


__all__ = [
    'DynaAPI',
    'DynaIncompleteComputationException',
    'DynaUnificationFailure'
]
