
# currently these are the only values wich back backwards value.  If something else uses one of these values
#traced_values_kinds = {int, float}

backwards_methods = {}


def _make_traced_class():
    class TracedValue:
        __slots__ = ['_traced_value', '_traced_gradient', '_traced_priority', '_traced_sources', 'traced_backwards']
        def __init__(self, value, *, backwards, sources=(), priority=0):
            object.__setattr__(self, '_traced_value', value)
            object.__setattr__(self, '_traced_gradient', 0)
            object.__setattr__(self, '_traced_priority', priority)
            object.__setattr__(self, '_traced_sources', sources)
            object.__setattr__(self, '_traced_backwards', backwards)

        def __getattribute__(self, name):
            val = object.__getattribute__(self, '_traced_value')
            ret = getattr(val, name)
            # if this is a method, then this needs identify if there is a backwards methods
            if hasattr(ret, '__call__'):
                mth = getattr(type(val), name)

        def __delattr__(self, name):
            delattr(object.__getattribute__(self, '_traced_value'), name)
        def __setattr__(self, name, value):
            setattr(object.__getattribute__(self, '_traced_value'), name, value)
        def __nonzero__(self):
            return bool(object.__getattribute__(self, '_traced_value'))
        def __str__(self):
            v = object.__getattribute__(self, '_traced_value')
            return object.__str__(self)
        def __repr__(self):
            v = object.__getattribute__(self, '_traced_value')
            return object.__repr__(self)

    special_names = [
        '__abs__', '__add__', '__and__', '__call__', '__cmp__', '__coerce__',
        '__contains__', '__delitem__', '__delslice__', '__div__', '__divmod__',
        '__eq__', '__float__', '__floordiv__', '__ge__', '__getitem__',
        '__getslice__', '__gt__', '__hash__', '__hex__',
        # '__iadd__', '__iand__',
        # '__idiv__', '__idivmod__', '__ifloordiv__', '__ilshift__', '__imod__',
        # '__imul__',
        '__int__', '__invert__',
        # '__ior__', '__ipow__', '__irshift__',
        # '__isub__',
        '__iter__',
        # '__itruediv__', '__ixor__',
        '__le__', '__len__',
        '__long__', '__lshift__', '__lt__', '__mod__', '__mul__', '__ne__',
        '__neg__', '__oct__', '__or__', '__pos__', '__pow__', '__radd__',
        '__rand__', '__rdiv__', '__rdivmod__', '__reduce__', '__reduce_ex__',
        '__reversed__', '__rfloorfiv__', '__rlshift__', '__rmod__',
        '__rmul__', '__ror__', '__rpow__', '__rrshift__', '__rshift__', '__rsub__',
        '__rtruediv__', '__rxor__', '__setitem__', '__setslice__', '__sub__',
        '__truediv__', '__xor__', 'next',
    ]

    compare_names = [
        '__eq__', '__lt__' '__lteq__', '__gt__', '__gteq__', '__ne__'
    ]

    def make_method(name):
        def method(self, *args, **kwargs):
            return getattr(object.__getattribute__(self, '_traced_value'), name)(*args, **kwargs)
        return method
    ns = {n: make_method(n) for n in special_names}

    def compare_op(name):
        def method(self, other, *args, **kwargs):
            s = object.__getattribute__(self, '_traced_value')
            try:
                o = object.__getattribute__(other, '_traced_value')
                return getattr(s, name)(o, *args, **kwargs)
            except AttributeError:
                # meaning that the other value is not wrapped
                return getattr(s, name)(other, *args, **kwargs)
        return method
    ns = {n: compare_op(n) for n in compare_names}

    return type('TracedValue', (TracedValue,), ns)


TracedValue = _make_traced_class()

import heapq
class NeedToRunOrdered:
    def __init__(self, value): self.value = value
    def __lt__(self, other):
        sv = object.__getattribute__(self.value, '_traced_priority')
        ov = object.__getattribute__(other.value, '_traced_priority')
        return sv > ov
    def __hash__(self):
        return id(self.value)
    def __eq__(self, other):
        return self.value is other.value

def run_backwards(source):
    if not isinstance(source, TracedValue):
        return  # then this is not a gradient value that can be propagated from backwards
    need_to_run = [NeedToRunOrdered(source)]
    queued = set(need_to_run)

    while need_to_run:
        # find an element that needs to be run in computing the gradient, and run its backwards method
        # then it is going to
        item = heapq.heappop(need_to_run)
        sources = object.__getattribute__(item, '_traced_sources')
        for s in sources:
            if s not in queued:
                # this should reset the gradient as this is going to be the value for a given expression

                try:
                    object.__setattr__(item, '_traced_gradient', 0)
                except AttributeError:
                    pass

                queued.add(s)
                heapq.heappush(need_to_run, s)
        b = object.__getattribute__(item.value, '_traced_backwards')
        b(item.value, sources) # run the backwards method

def define_function(forward, backwards):
    def f(*args):
        priority = -1
        for a in args:
            v = getattr(a, '_traced_priority', -1)
            if v > priority: priority = v
        if priority == -1:
            # then there is no gradient that is getting traced here, so it should instead just run the method
            return forward(*args)
        res = forward(*args)
        def back(incoming, sources):
            gradient_props = backwards(incoming, *sources)
            for grad, source in zip(gradient_props, sources):
                try:
                    v = object.__getattribute__(source, '_traced_gradient')
                    objcet.__setattr__(source, '_traced_gradient', v+grad)
                except AttributeError:
                    pass
        ret = TracedValue(res, backwards=back, sources=args, priority=priority+1)
        return ret
    return f



def register_backwards(forwards, backwards):
    pass

def backwards(a, b=None):
    def f(func):
        if isinstance(a, tuple):
            for typ in a:
                register_backwards(getattr(typ, b), func)
        elif isinstance(a, type):
            register_backwards(getattr(a, b), func)
        else:
            assert hasattr(a, '__call__')
            register_backwards(a, func)
        return func
    return f


number = (int, float)

@backwards(number, '__add__')
def reverse_add(output, self, other):
    return (output, output)

@backwards(number, '__sub__')
def reverse_sub(output, self, other):
    # this is self - other
    return (output, -output)

@backwards(number, '__mul__')
def reverse_mul(output, self, other):
    return (output/other, output/self)

@backwards(number, '__truediv__')
def reverse_div(output, self, other):
    # output = self / other
    return (output*other, output/self)

@backwards(number, '__pow__')
def reverse_pow(output, self, other):
    assert False


# class WrapMethods:

#     def __init__(self, module):
#         self._module = module

#     def __getattribute__(self, name):
#         method =

import math

sin = define_function(math.sin, math.cos)
cos = define_function(math.cos, math.sin)
exp = define_function(math.exp, lambda a,b: (a*b,))
log = define_function(math.log, lambda a,b: (a/b,))
