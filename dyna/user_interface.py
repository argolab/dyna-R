import threading
import inspect
import numbers
import json
import dis
import uuid
from collections import defaultdict
import re
import textwrap
import os

from .delayed_value import DynaDelayedValue

_active_dynabase = threading.local()

def _get_active():
    global _active_dynabase
    if getattr(_active_dynabase, 'l', None) is None:
        _active_dynabase.l = []
        _active_dynabase.txn = None
        _active_dynabase.constructed_symbols = None  # changed into an array by the aggregator decorator
    return _active_dynabase


class Dyna(object):

    def __init__(self):
        self.dyna_id = uuid.uuid4().hex
        get_server().create_engine(self.dyna_id)

    def __del__(self):
        get_server().destroy_engine(self.dyna_id)

    def __enter__(self):
        assert _get_active().txn is None, "Unable to enter a root dyna object from inside of a transaction"
        self.begin().__enter__()

    def __exit__(self, *args):
        txn = _get_active().txn
        assert txn.dynabase is self, "exiting the wrong dynabase for this transaction"
        txn.__exit__(*args)

    def begin(self):
        return DynaTransaction(self)

    def txn(self):
        return self.begin()

    def import_file(self, fname):
        assert False
        if _get_active().txn is None:
            raise NotInTransaction()
        if hasattr(fname, 'name'):
            fname = fname.name
        # this should check that this is running on local host, otherwise we
        # should maybe just read the file and add it to the terms list?  but
        # then we are also going to have to deal with nested import statements
        # again....sigh
        _get_active().txn.terms.append(
            f':- import "{os.path.abspath(fname)}'
        )

    def __getattribute__(self, name):
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            txn = _get_active().txn
            if txn is None:
                raise NotInTransaction('access properties')
            #assert _get_active().txn is not None, "only able to access properties of a dynabase from inside of a transaction"
            assert txn.dynabase is self, "Trying to access a property on another dynabase from within the wrong transaction"
            return DynaAttributeObject(name)


class DynaTransaction(object):

    def __init__(self, dynabase):
        self.dynabase = dynabase

        # terms that are going to be added to the program
        self.terms = []
        # updates into the dyna engine, should mostly be in the form of constant values
        self.updates = defaultdict(list)
        # anything that we are going to query against
        self.queries = defaultdict(lambda: defaultdict(DynaDelayedValue))

        self.update_aggregators = {}

    def __enter__(self):
        assert _get_active().txn is None
        _get_active().txn = self
        _get_active().l.append(self.dynabase)

    def __exit__(self, *args):
        assert _get_active().txn == self
        assert _get_active().l[-1] == self.dynabase
        _get_active().txn = None
        del _get_active().l[-1]

        # only run the commit on exit if this wasn't caused by some error inside of the block
        if len(args) > 0 and args[0] is None:
            self.commit(start_new=False)

    # def add_expression(self, expr_str):
    #     "Add an expression string to this dyna base"
    #     self.terms.append(expr_str)

    def add_program(self, expr_str):
        "Add multiple expressions to this dynabase as a string of code"
        self.terms.append(expr_str)

    def rollback(self):
        self.terms = []
        self.updates = defaultdict(list)
        self.queries = defaultdict(lambda: defaultdict(DynaDelayedValue))
        self.update_aggregators = {}

    def commit(self, *, start_new=True):
        # check that there is something that we need to actually do, otherwise just exit
        if len(self.terms) != 0 or len(self.updates) != 0 or len(self.queries) != 0:
            updates = []
            for (name, nargs), values in self.updates.items():
                aggregator = self.update_aggregators.get((name, nargs), "=")
                for (keys, val) in values:
                    updates.append("'{}'({}) {} {}.".format(
                        name,
                        ', '.join(map(str, keys)),
                        aggregator,
                        val
                    ))


            program_str = '\n'.join(self.terms + updates)
            print(program_str)

            get_server().submit_txn(self.dynabase.dyna_id, program_str, self.queries)

        # we perform a rollback after we have been committed, as all of the pending values should have been filled in
        # as well as all all updates submitted into the system
        self.rollback()


def _make_key(name):
    if not isinstance(name, tuple):
        if hasattr(name, '__iter__'):
            name = tuple(name)
        else:
            # this is a single value but still make a tuple
            name = (name,)
    for n in name:
        if not (isinstance(n, numbers.Number) or isinstance(n, str)):
            raise DynaTypeException(n)
        #assert isinstance(n, numbers.Number) or isinstance(n, str), "Arguments are not fully ground, do not support free values in the queries and updates yet at this time"
    return name


class DynaAttributeObject(object):

    def __init__(self, name):
        self.name = name

    def __enter__(self):
        # TODO: having more than one dynabase
        assert False
        _get_active().l.append(self)

    def __exit__(self, *args):
        assert _get_active().l[-1] == self
        del _get_active().l[-1]

    def __getattribute__(self, name):
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            # for accessing some dynabase
            assert False
            # TODO: having some nested dynabases or something

    def __call__(self, *args):
        return self.__getitem__(args)

    def __getitem__(self, name):
        # for reading some field out of a table
        name = _make_key(name)
        return _get_active().txn.queries[(self.name, len(name))][name]

    def __setitem__(self, name, value):
        # for writing some update into a table
        name = _make_key(name)
        _get_active().txn.updates[(self.name, len(name))].append((name, value))


class DynaSymbolic(object):

    def __init__(self, name, *arguments, is_constant=False, uname=None, quote=False):
        self.name = name
        self.arguments = []
        self.is_constant = is_constant
        self.is_used = False
        self.quote = quote
        self.uname = uname
        if self.is_constant:
            assert len(self.arguments) == 0
            if isinstance(name, numbers.Number):
                self.name = str(name)
            elif isinstance(name, str):
                # the json encoder is for escaping the string
                self.name = json.encode(name)
            elif name is None:
                self.name = "$null"
                self.is_constant = True
            else:
                # IDK what do here???
                print("Unable to figure the type out for {} {}".format(name, type(name)))
                assert False

        for a in arguments:
            if isinstance(a, DynaSymbolic):
                self.arguments.append(a)
            else:
                self.arguments.append(DynaSymbolic(a, is_constant=True))

        for a in self.arguments:
            a.is_used = True

        _get_active().constructed_symbols.append(self)

    def __add__(self, other):
        return DynaSymbolic("+", self, other)

    def __sub__(self, other):
        return DynaSymbolic("+", self, other)

    def __mul__(self, other):
        return DynaSymbolic("*", self, other)

    def __div__(self, other):
        return DynaSymbolic("/", self, other)

    def __eq__(self, other):
        # this is going to be a unification, which will generate a slightly different output..
        return DynaSymbolic("=", self, other)

    def __lt__(self, other):
        return DynaSymbolic("<", self, other)

    def __le__(self, other):
        return DynaSymbolic("<=", self, other)

    def __gt__(self, other):
        return DynaSymbolic(">", self, other)

    def __ge__(self, other):
        return DynaSymbolic(">=", self, other)

    def __call__(self, *args):
        assert len(self.arguments) == 0
        if self.name[0] == '_':
            # let an underscore as the first symbol in a name represent quoting
            return DynaSymbolic(self.name[1:], *args, quote=True)
        else:
            return DynaSymbolic(self.name, *args)

    def __bool__(self):
        s = ('Unable to use a dyna expression inside of a loop or conditional statement in python\n'
              'please see how aggregation in dyna and checking for equalities will allow for these expressions')
        print(s)
        raise NotImplementedError(s)

    def __iter__(self):
        s = ('Unable to use a dyna expression inside of a loop or conditional statement in python\n'
              'please see how aggregation in dyna and checking for equalities will allow for these expressions')
        print(s)
        raise NotImplementedError(s)

    def create_program(self, i, prog):
        if self.uname is None:
            self.uname = 'VAR_{}'.format(i)
            i += 1
            for a in self.arguments:
                i = a.create_program(i, prog)
            if len(self.arguments) == 0:
                prog.append("{} is {}".format(self.uname, self.name))
            else:
                if self.name == '=' and len(self.arguments) == 2:
                    # then this is the unification, which we have to use a different syntax for
                    prog.append("{} is ({} = {})".format(self.uname, self.arguments[0].uname, self.arguments[1].uname))
                else:
                    prog.append("{} is '{}'({})".format(self.uname, self.name, ','.join([n.uname for n in self.arguments])))
        return i

    def create_rexpr(self, prog):
        if self.uname is None:
            self.uname = ('VAR', object())
            for a in self.arguments:
                a.create_rexpr()
            if self.name == '=' and len(self.arguments) == 2:
                # make this a unify operation
                prog.append(Unify(self.arguments[0].uname, self.arguments[1].uname, self.uname))
            else:
                prog.append(CallTerm(self.uname, [self.name] + [a.uname for a in self.arguments]))
        return self.uname

    def __str__(self):
        if len(self.arguments) == 0:
            return self.name
        else:
            return "{}({})".format(self.name, ', '.join([str(n) for n in self.arguments]))

    def __repr__(self):
        return self.__str__()


def get_function_body(aggregator_name, func):
    name = func.__name__
    args = inspect.getfullargspec(func)
    if not (args.varargs is None and args.varkw is None and args.defaults is None):
        raise DynaDSLException('Dyna does not support variable number of arguments, keyword arguments, or defaults')

    _get_active().constructed_symbols = []

    mth_sym = DynaSymbolic(name)
    arg_map = [DynaSymbolic(name='ARG--{}'.format(i), uname="ARG_{}".format(i)) for i, n in enumerate(args.args)]

    # TODO: grab the local values which might have been captured in some closure with this in the case that we want to allow using
    # those inside of some rule
    global_items = {}

    bc = dis.Bytecode(func)
    for b in bc:
        if b.opname in ('LOAD_GLOBAL', 'LOAD_DEREF'):
            # then there is something that is not defined locally inside of the
            # method, so we are going to override it with our symbolic stuff
            n = b.argval
            if n not in global_items:
                global_items[n] = DynaSymbolic(n)


    # rewrite the consturcted symbols as these should just be variables that were used so far
    _get_active().constructed_symbols = []

    # there should already be some annotation on the function, which is how we got called in the first place
    func_source = inspect.getsource(func)
    func_source = re.sub(r'@[a-zA-Z][a-zA-Z\.0-9]*(\(.*\))', '@dyna_get_function', func_source)

    func_source = textwrap.dedent(func_source)

    if '@dyna_get_function' not in func_source:
        func_source = '@dyna_get_function\n'+func_source

    func_holder = [None]

    def get_function(*args, **_):
        if len(args) > 0:
            func_holder[0] = args[0]
        return mth_sym

    global_items['dyna_get_function'] = get_function

    # re-eval the function using its source definition such that we do not conflict with the scope that it was defined in
    exec(func_source, global_items)

    # check that we actually got some reference to a function based
    assert func_holder[0] and hasattr(func_holder[0], '__call__'), "failed to get function definition"

    # here we are actually calling the function that we constructed
    result = func_holder[0](*arg_map)

    # fix up the final value if a constant
    if not isinstance(result, DynaSymbolic):
        result = DynaSymbolic(result, is_constant=True)

    # I suppose that we could just take the last thing that we constructed instead of requring that
    # we return the last item.  But then we wouldn't be able to distinguish between None that is
    # returned in those cases and when something actually wants to return none

    result.is_used = True
    prog = []
    i = 0
    for a in _get_active().constructed_symbols:
        if not a.is_used:
            i = a.create_program(i, prog)
            # append the uname as we want to assert that this statement is true
            prog.append(a.uname)

    i = result.create_program(i, prog)

    prog.append(result.uname)
    _get_active().constructed_symbols = None

    # string_rep = "'{name}'({args}) {agg} {body} .".format(
    #     name=name,
    #     args=','.join([n.uname for n in arg_map]),
    #     agg=aggregator_name,
    #     body=',\n'.join(prog)
    # )

    return (name, args, aggregator_name, body)



def aggregator(aggregator_name):
    # we won't be able to detect the case where there are two vlaues which contribute to the same item if they forget the aggregator
    # annotation, that is just a slient error in python.... and the last one defined will overwrite everything else

    # also we can not identify which class is getting created, which means that if there were to be nested aggregands then we could not
    # look at what is present?

    # might be better if we just had some method and used with clauses

    def f(func):
        body = get_function_body(aggregator_name, func)
        _get_active().txn.terms.append(body)
        # this method isn't actually used in the body, so just fill with junk?
        return None

    return f


def add_program(program_str):
    _get_active().txn.add_program(program_str)


def create_parameter(name, nargs):
    _get_active().txn.add_program(
        ":- parameter_container '{}'/{}.".format(name, nargs)
    )

def create_table(name, nargs, aggregator="="):
    agg = _get_active().txn.update_aggregators.get((name, nargs))
    if agg is not None:
        assert agg == aggregator
    else:
        _get_active().txn.update_aggregators[(name, nargs)] = aggregator


def commit():
    _get_active().txn.commit()
