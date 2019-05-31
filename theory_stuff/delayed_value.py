DynaUnknownValue = object()

def _make_proxy_class():
    class DynaDelayedValue(object):
        """
        Represents a delayed value that we are going to fill in after the transaction has been commited

        Based off: http://code.activestate.com/recipes/496741-object-proxying/
        """

        __slots__ = ['_value']

        def __init__(self):
            object.__setattr__(self, '_value', DynaUnknownValue)

        def __getattribute__(self, name):
            return getattr(object.__getattribute__(self, "_value"), name)
        def __delattr__(self, name):
            delattr(object.__getattribute__(self, "_value"), name)
        def __setattr__(self, name, value):
            setattr(object.__getattribute__(self, "_value"), name, value)
        def __nonzero__(self):
            return bool(object.__getattribute__(self, "_value"))
        def __str__(self):
            v = object.__getattribute__(self, "_value")
            if v is not DynaUnknownValue:
                return str(v)
            return object.__str__(self)
        def __repr__(self):
            v = object.__getattribute__(self, "_value")
            if v is not DynaUnknownValue:
                return repr(v)
            return object.__repr__(self)

    # we are just going to add all of these to the proxy class
    # I suppose that these could only be added after the fact once the value is set
    # this would
    special_names = [
        '__abs__', '__add__', '__and__', '__call__', '__cmp__', '__coerce__',
        '__contains__', '__delitem__', '__delslice__', '__div__', '__divmod__',
        '__eq__', '__float__', '__floordiv__', '__ge__', '__getitem__',
        '__getslice__', '__gt__', '__hash__', '__hex__', '__iadd__', '__iand__',
        '__idiv__', '__idivmod__', '__ifloordiv__', '__ilshift__', '__imod__',
        '__imul__', '__int__', '__invert__', '__ior__', '__ipow__', '__irshift__',
        '__isub__', '__iter__', '__itruediv__', '__ixor__', '__le__', '__len__',
        '__long__', '__lshift__', '__lt__', '__mod__', '__mul__', '__ne__',
        '__neg__', '__oct__', '__or__', '__pos__', '__pow__', '__radd__',
        '__rand__', '__rdiv__', '__rdivmod__', '__reduce__', '__reduce_ex__',
        '__reversed__', '__rfloorfiv__', '__rlshift__', '__rmod__',
        '__rmul__', '__ror__', '__rpow__', '__rrshift__', '__rshift__', '__rsub__',
        '__rtruediv__', '__rxor__', '__setitem__', '__setslice__', '__sub__',
        '__truediv__', '__xor__', 'next',
    ]

    def make_method(name):
        def method(self, *args, **kwargs):
            return getattr(object.__getattribute__(self, '_value'), name)(*args, **kwargs)
        return method

    ns = {n: make_method(n) for n in special_names}

    return type('DynaDelayedValue', (DynaDelayedValue,), ns)

DynaDelayedValue = _make_proxy_class()


def set_delayed_value(self, value):
    assert type(self) is DynaDelayedValue
    object.__setattr__(self, '_value', value)
