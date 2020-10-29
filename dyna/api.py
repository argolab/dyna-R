# a public api for dyna to make working with the system a bit easier hopefully

import re
import inspect

from dyna import context
from dyna.interpreter import saturate, loop, Frame, Terminal, ret_variable, VariableId, UnificationFailure as DynaUnificationFailure, constant, Unify, unify, intersect, partition, ConstantVariable, Aggregator
from dyna.builtins import moded_op
from dyna.syntax.normalizer import user_query_to_rexpr, run_parser, FVar as ParserWrappedVariableName
from dyna.optimize import run_optimizer
from dyna.terms import CallTerm, Term, BuildStructure
from dyna.aggregators import AGGREGATORS, colon_line_tracking
#from dyna.prefix_trie import PrefixTrie


class WrappedOpaqueObject:
    # make it so that the value has a __hash__ and __eq__ method so that we can insert it into the trie etc
    # though with something like a dict, this is going to not be based on the actual value but rather the pointer identity...
    __slots__ = ('_value')
    def __init__(self, value): self._value = value
    def __hash__(self): return id(self._value)
    def __eq__(self, other): return isinstance(other, WrappedOpaqueObject) and self._value is other._value
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
            # if it not a list then .aslist either returns None or raises an exception (at some point internally)
            # iterating over the None value returns TypeError so that should still be caught
            return [cast_from_dyna(v) for v in value.aslist()]
        except (TypeError,AttributeError):
            pass
    elif isinstance(value, WrappedOpaqueObject):
        return value._value
    return value


assignment_colon_name = VariableId('$ASSIGNMENT_COLON_TRACKING')
assignment_colon_value = VariableId('$ASSIGNMENT_COLON_VALUE')

def construct_call(system, string):
    # given a string like `foo/123` or `foo(%,%,7, bar(8, %))`, construct an
    # R-expr which can be used to retrieve the value of the expression.  The
    # variables 0,1,... will be the arguments and ret_variable will be the returned variable

    m = re.match(r'([a-z][a-zA-Z0-9_\$]*)/([0-9]+)', string)
    if m:
        name = m.group(1)
        arity = int(m.group(2))
        return system.call_term(name, arity), arity, (name, arity)
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
        VariableId(ParserWrappedVariableName('Result')): ret_variable,
    }
    for i in range(var_idx):
        rep[VariableId(ParserWrappedVariableName(f'ARGUMENT_{i}'))] = VariableId(i)
    rexpr = user_query_to_rexpr(s, dyna_system=system)
    rexpr = rexpr.rename_vars_unique(rep.get)
    # this needs to determine which of the expressions is the outer most one, so that we can report its name and arity
    # so an expression like foo(%, 123) will have that there is one variable, but calls foo/2
    call_var_ret = {}
    unified_vars = {}
    for child in rexpr.all_children():
        if isinstance(child, CallTerm):
            for var in child.var_map.values():
                call_var_ret[var] = child
        if isinstance(child, Unify):
            unified_vars.setdefault(child.v1, []).append(child.v2)
            unified_vars.setdefault(child.v2, []).append(child.v1)
    call_term = None
    def s(v):
        nonlocal call_term
        if v in call_var_ret:
            call_term = call_var_ret[v]
            return True
        for x in unified_vars[v]:
            if s(x): return True
        return False
    s(ret_variable)

    assert call_term is not None
    name = call_term.term_ref
    if not (isinstance(name, tuple) and len(name) == 2 and isinstance(name[0], str) and isinstance(name[1], int)):
        name = None

    return rexpr, var_idx, name

def convert_to_assignment(system, rexpr, arity):
    call_var_ret = {}
    unified_vars = {}

    name_rename = {VariableId(i): VariableId(f'$ARGUMENT_{i}') for i in range(arity)}
    rexpr = rexpr.rename_vars(lambda x: name_rename.get(x,x))

    for child in rexpr.all_children():
        if isinstance(child, CallTerm):
            for var in child.var_map.values():
                call_var_ret[var] = child
        if isinstance(child, Unify):
            unified_vars.setdefault(child.v1, []).append(child.v2)
            unified_vars.setdefault(child.v2, []).append(child.v1)
    call_term = None
    def s(v):
        nonlocal call_term
        if v in call_var_ret:
            call_term = call_var_ret[v]
            return True
        for x in unified_vars[v]:
            if s(x): return True
        return False
    s(ret_variable)

    assert call_term is not None  # this is what is going to become the head of the expression

    def remove_call(r):
        if r is call_term:
            return Terminal(1)
        else:
            return r.rewrite(remove_call)
    rexpr = remove_call(rexpr)

    rexpr_add = []
    for a,b in call_term.var_map.items():
        if isinstance(a._compiler_name, int):
            rexpr_add.append(unify(a,b))
        else:
            assert a is ret_variable and b is ret_variable
    rexpr = intersect(rexpr, *rexpr_add)

    # rexpr = intersect(rexpr, *(unify(a,b) for a,b in call_term.var_map.items()))
    call_name, call_arity = call_term.term_ref

    iret = VariableId()
    #renamed[ret_variable] = iret
    rexpr = intersect(BuildStructure('$colon_line_tracking', iret, (assignment_colon_name, assignment_colon_value)), rexpr)
    args = tuple(VariableId(i) for i in range(call_arity))

    rexpr = Aggregator(ret_variable,
                       args,
                       iret,
                       AGGREGATORS[':='],
                       partition((*args, iret), [rexpr]))

    return rexpr

def make_call_proxy(system, source, dest, arity):
    r = system.lookup_term((dest, arity), ignore=('memo', 'not_found', 'assumption', 'assumption_defined'))
    if r == Terminal(0):
        # meaning that this is not defined, so we are going to construct a new call for this
        c = system.call_term(source, arity)
        # pt = PrefixTrie(arity+1)
        # pt[(None,)*(arity+1)] = [c]
        p = partition(tuple(VariableId(i) for i in range(arity)) + (ret_variable,), [c])
        system.define_term(dest, arity, p)

# def construct_assignment(system, string):
#     raise NotImplemented()

#     m = re.match(r'([a-z][a-zA-Z0-9_\$]*)/([0-9]+)', string)
#     if m:
#         name = m.group(1)
#         arity = int(m.group(2))
#         agg = system.lokup_term_aggregator(name, arity)
#         # if aggregator:
#         #     agg, _ = agg
#         # else:
#         #     agg =
#         # this should construct a dummy expression where it just has the aggregtor for the given variables which are in the expression
#         # then it will find that there are values which correspond with
#         vals = [Unify(VariableId(i), VariableId(f'ARGUMENT_{i}')) for i in range(arity)] + [Unify(ret_variable, VariableId('VALUE'))]


#         return system.call_term(name, arity), arity, (name, arity)



class DynaIncompleteComputationException(Exception):

    def __init__(self, rexpr):
        super().__init__('Computation was incomplete, use `api.expose_rexprs = True` to see the internal result')
        self.rexpr = rexpr


class DynaExpressionWrapper:
    # this will wrap a single method or statement in the program.  So that it can

    def __init__(self, api, *, statement, rexpr=None, arity=None):
        self._api = api
        if statement is not None:
            self._statement = statement
            self._call, self._arity, self._name = construct_call(api._system, statement)
        else:
            self._statement = None
            self._call = rexpr
            self._arity = arity
            self._name = None
        self._assignment_cache = None

    def __call__(self, *args):
        self._api._check_run_agenda()
        assert len(args) == self._arity, "the number of arguments does not match"
        frame = Frame()
        incomplete_vars = []
        for i,a in enumerate(args):
            if a == slice(None):
                # this means that the expression is something like [1,2,:] where the result is some R-expr that still requires looping over the values
                incomplete_vars.append(VariableId(i))
            else:
                # this is a value in the language, so we are going to set that into the frame
                frame[i] = cast_to_dyna(a)
        r = saturate(self._call, frame)
        if incomplete_vars:
            # then this is something that we are going to loop over still, so we return another wrapper
            var_map = {f: VariableId(i) for i, f in enumerate(incomplete_vars)}
            def rv(x):
                if x.isBound(frame):
                    return constant(x.getValue(frame))
                return var_map.get(x,x)
            new_r = r.rename_vars(rv)
            for f, t in var_map.items():
                if f.isBound(frame):
                    new_r = intersect(Unify(t, constant(f.getValue(frame))), new_r)
            return DynaExpressionWrapper(self._api, statement=None, rexpr=new_r, arity=len(incomplete_vars))

        if r == Terminal(0):
            return None
        if r == Terminal(1):
            return cast_from_dyna(ret_variable.getValue(frame))
        if self._api._expose_rexprs:
            return r
        else:
            raise DynaIncompleteComputationException(r)

    def __getitem__(self, args):
        if not isinstance(args, tuple):
            return self.__call__(args)  # in python with a[1,2,3] this is a single argument passed as a tuple...
        return self.__call__(*args)   # I think that this is basically going to look the same for us in dyna as the call

    def __setitem__(self, key, value):
        if not isinstance(key, tuple):
            key = key,  # make it always a tuple
        assert self._name, "can not set the value of an expression which does not have a user referencable name"
        assert self._arity == len(key), "The number of arguments getting set into the table does not match"

        value = cast_to_dyna(value)
        key = tuple(cast_to_dyna(k) for k in key)

        if self._assignment_cache is None:
            self._assignment_cache = convert_to_assignment(self._api._system, self._call, self._arity)
        var_map = {
            assignment_colon_name: constant(colon_line_tracking()),
            assignment_colon_value: constant(value)
        }
        for i, v in enumerate(key):
            var_map[VariableId(f'$ARGUMENT_{i}')] = constant(v)
        def rename(x):
            if isinstance(x._compiler_name, int) or x is ret_variable:
                return x
            return var_map.get(x,x)
        assignment = self._assignment_cache.rename_vars_unique(rename)

        self._api._system.add_to_term(self._name[0], self._name[1], assignment)

        # this needs to figure out which aggregator an expression is using? and then override the value
        # or we could always just use := for expressions that are assigned via this setitem

        # this should delcare a new rule into the program.  It will want to cause that
        # if this just defines an expression using :=, then it would
        #raise NotImplemented()

    def callback(self, cb):
        self._api._check_run_agenda()
        # this is going to take the expression and loop over its different and callback for each of the values
        frame = Frame()
        user_vars = [VariableId(i) for i in range(self._arity)] + [ret_variable]
        r = self._call
        if self._api._auto_run_optimizer:
            r, _ = run_optimizer(r, user_vars)
        r = saturate(r, frame)

        def mapper(rr, ff):
            if not isinstance(rr, Terminal):
                if self._api._expose_rexprs:
                    cb(rr)
                else:
                    raise DynaIncompleteComputationException(rr)
            if rr.isEmpty(): return
            *arg_values, res_value = [cast_from_dyna(v.getValue(ff)) for v in user_vars]
            arg_values = tuple(arg_values)
            for _ in range(rr.multiplicity):
                cb((arg_values, res_value))

        loop(r, frame, mapper, best_effort=True)

    loop_via_callback = callback

    def __iter__(self):
        return callback_to_iterator(self.callback)

    def to_dict(self):
        ret = {}
        @self.loop_via_callback
        def s(x): ret[x[0]] = x[1]
        return ret

    def set_memoized(self, mode):
        assert mode in ('null', 'unk', 'none')
        assert self._name is not None and isinstance(self._name, tuple) and len(self._name) == 2

        self._api._system.memoize_term(self._name, kind=mode)

        # import ipdb; ipdb.set_trace()
        # raise NotImplemented()

    def __repr__(self):
        return str(self)
    def __str__(self):
        if self._statement:
            return str(self._statement)
        return str(self._call)

class DynaAPI:

    def __init__(self, program=None):
        self._system = context.SystemContext()

        # the auto run agenda should call the agenda method before it makes any queries into the program
        self._auto_run_agenda = True
        self._auto_run_optimizer = True
        self._expose_rexprs = False

        self._tables = {}

        if program:
            self.add_rules(program)

    ##################################################
    # properties which influence how the api and system runs

    @property
    def auto_run_agenda(self): return self._auto_run_agenda

    @auto_run_agenda.setter
    def auto_run_agenda(self, val):
        self._auto_run_agenda = bool(val)

    @property
    def auto_run_optimizer(self): return self._auto_run_optimizer

    @auto_run_optimizer.setter
    def auto_run_optimizer(self, val):
        self._auto_run_optimizer = bool(val)

    @property
    def expose_rexprs(self): return self._expose_rexprs

    @expose_rexprs.setter
    def expose_rexprs(self, val):
        self._expose_rexprs = bool(val)

    @property
    def stack_recursion_limit(self): return self._system.stack_recursion_limit

    @stack_recursion_limit.setter
    def stack_recursion_limit(self, val):
        assert val > 0
        self._system.stack_recursion_limit = val

    ##################################################
    # api calls for interacting with the system

    def call(self, method, *args):
        # use this like foo/2 or foo(%,%) where it would identify that the %
        # symbols should be replaced with placeholder variables for some
        # expression
        c = self.make_call(method)
        return c(*args)

    def make_call(self, method):
        # if it matches the expression where it would have some expression that corresponds with
        return DynaExpressionWrapper(self, statement=method)

    def table(self, name, arity):
        key = (name, arity)
        if key not in self._tables:
            make_call_proxy(self._system, f'{name}_value_defined_table', name, arity)
            self._system.memoize_term((f'{name}', arity), kind='null', mem_variables=()) #tuple(VariableId(i) for i in range(arity)))

            # then this is going to want to create some table which corresponds with this expression
            pass
        # this creates a table which can be set directly from dyna, though it would have
        r = DynaExpressionWrapper(self, statement=f'{name}_value_defined_table/{arity}')
        #r.set_memoized('null')
        return r

    def define_function(self, name=None, arity=None):
        """This could be used as:

        @api.define_function()
        def my_function(arg1, arg2):
            return arg1+arg2

        then my_function/2 would be exposed in the dyna runtime for the all ground mode.

        This is done just using the builtins to expose these methods simply.
        """
        def f(func):
            lname, larity = name, arity
            if lname is None:
                lname = func.__name__
            if larity is None:
                larity = len(inspect.getfullargspec(func).args)
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

    ##################################################
    # private methods

    def _check_run_agenda(self):
        if self._auto_run_agenda:
            self.run_agenda()


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


# DISCUSSION: could write a garbage collect pass for := aggregator.  Then when
# setting values for an expression, it can just always use :=.  This way, it
# could allow for "advanced" expressions where there could be any R-expr, while
# still not being /too/ inefficient with storing all of the old values for some
# expression.
#
# there is also a version where an expression would just be a simpler table.
# Then it would allow for the values to pass the updates down in a more fine
# grained manor.  This will mean that the values
