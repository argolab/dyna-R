################################################################################
# Aggregation data structures
#
#  - 2.3.4.1 Selective Memoization -- you don't have to store the actual object.
#    You might want to store something else.
#
#    If the framework needs to reclaim space, one could imagine a progression of
#    @flush⟨x⟩ messages, describing how much of the agent’s internal state is
#    believed to be of future utility to the larger computation.  One could ask
#    to flush cached results, trim aggregation trees to their tops, to decrease
#    the size in a selective aggregator’s associative map, etc. However, if
#    resources are truly tight, the framework is free to discard the entire
#    state object and begin again should j receive a message.  More generally,
#    while we have, thus far, considered only queries for an item’s value in its
#    entirety, as an opaque quantity. However, we may wish to allow for
#    predicated queries, e.g., “Is your value positive?” These kinds of queries
#    can be answered accurately even if agents are storing partial information
#    about their values. More generally, while we have, thus far, considered
#    only queries for an item’s value in its entirety, as an opaque
#    quantity. However, we may wish to allow for predicated queries, e.g., “Is
#    your value positive?” These kinds of queries can be answered accurately
#    even if agents are storing partial information about their values.
#
#      - [2019-02-06 Wed] this completely simpatico with my recent "epiphany"
#        about aggregation data structures.  Sometimes retraction is supported,
#        sometimes it isn't, but fear not, we can fallback to refreshes.
#
#        NWF's commentary above suggests that we can take things every further!
#        maybe we don't even store the value in the memo!  We can just store
#        "properties" of the memo.
#
#      - A common example of "properties of memos" are indexes, which might only
#        store of the item is nonnull (not its actual value).
#
################################################################################

# TODO: Technically, we need to "name" our `inf` values by their hyperedge if we
# want to be able to retract them.  Currently, we get a NaN from inf-inf
# multiplicity.

# TODO: Need to map values into a dyna-friendly format (e.g., list/set -> cons-list)

import numpy as np
from collections import Counter

from dyna.syntax.generic import null_ptr, deref, Term, rgensym, has_err, pp
from dyna.syntax.exceptions import AggregatorMismatch, AggregatorValueError


ALLOW_INVERSE = True


# TODO: using `equal` during propagation results in bigger errors
# downstream.  Are there approximate equality metrics which are
# better for this type of thing?
def equal(e, g):
    assert not isinstance(e, Aggr) and not isinstance(g, Aggr), [e, g]
    if isinstance(e, (int, float)) and isinstance(g, (int, float)):
        if e == 0: return e == g   # expecting an actual zero, might be a bad assumption.
        return abs(e - g) / abs(e) < 0.01    # less than 1% relative error
    else:
        return e == g


def has_unk(x):
    x = deref(x)
    if isunk(x):
        return True
    elif isinstance(x, Term):
        return any(map(has_unk, x))


def upcast_unk(x):
    "Upcast term `x` to a more general term by replacing UNKs with free variables."
    x = deref(x)
    if isunk(x):
        return rgensym()   # upcast
    elif isinstance(x, Term):
        return Term(*map(upcast_unk, x.fargs))
    else:
        return x


def deref_value(item, v):
    #assert v is None or isinstance(v, Aggr) or isunk(v), [item, v]   # implies not a `Result`
    if isinstance(v, Aggr):
        try:
            return v.value
        except AggregatorValueError as e:
            raise AggregatorValueError(f'`{item}` -> `{e}`')
    return v


class _unk:
    def __repr__(self):
        return '$unk'
    @property
    def value(self):
        return self
    def __hash__(self):
        return 0
    def __eq__(self, other):
        return isunk(other)
    def has_unk(self):
        return True
    @property
    def default(self):
        return self
    def merge_helper(self, other):
        from dyna.answer import Result
        return Result(default=UNK).merge_helper(other)
    def __neg__(self):
        return self


def isunk(v):
    #from dyna.answer import Result
    #assert not isinstance(v, Result)
    if isinstance(v, Aggr): v = v.value
    return v is UNK or isinstance(v, _unk)

UNK = _unk()


class Aggr:

    def fold(self):
        raise NotImplementedError

    @property
    def value(self):
        self.check()
        # TODO: value should always "succeed" by returning $error when the
        # underlying computation doesn't succeed. Similary to inf, $error is a
        # "hyperedge named" value. (Come to think of it, that might be how we
        # could sneak in randomness).
        return self.fold()

    def increment(self, v, m):
        raise NotImplementedError

    def check(self):
        pass

    def __hash__(self):
        assert False, 'Aggr is unhashable'

    def __eq__(self, other):
        if isinstance(other, Aggr): other = other.value
        #return equal(self.value, other)
        return self.value == other

    def __iadd__(self, y):
#        assert isinstance(y, Aggr), y
        if not isinstance(self, y.__class__):
            raise AggregatorMismatch(f'Conflicting aggregators `{self}` and `{y}`.')
        for v, m in y.bag.items():
#            if v is not None:
            self.increment(v, m)
        return self

    def __add__(self, y):
        z = self.__class__()
        z += self
        z += y
        return z

    def __radd__(self, y):
        if y is None: return self   # special handling for (None + Aggr)
        raise ValueError(f'expected Aggr, got {y}')


class BAggr(Aggr):

    def __init__(self, vs=None):
        self.bag = Counter()
        if vs is not None:
            if isinstance(vs, list):
                for v in vs:
                    self.increment(v, +1)
            else:
                assert isinstance(vs, dict)
                for v, m in vs.items():
                    self.increment(v, m)

    def supports_inverse_elements(self):
        return False

#    def clear(self):
#        self.bag.clear()

    @property
    def value(self):
        self.check()
        if not self.bag: return None

        if any(map(has_err, self.bag)): return Term('$error')

        if any(isunk(v) for v,m in self.bag.items() if m): return UNK
        #assert not any(isunk(v) for v,m in self.bag.items() if m)

        # TODO: value should always "succeed" by returning $error when the
        # underlying computation doesn't succeed. Similary to inf, $error is a
        # "hyperedge named" value. (Come to think of it, that might be how we
        # could sneak in randomness).
        return self.fold()

    def __neg__(self):
        n = self.__class__()
        for k, v in self.bag.items():
            n.bag[k] = -v
        return n

    def increment(self, v, m):
        if v is None: return
        self.bag[v] += m
        if self.bag[v] == 0: del self.bag[v]
#        self.check()

    def check(self):
        if self.supports_inverse_elements(): return
        for v in self.bag:
            if self.bag[v] < 0:
                raise AggregatorValueError(self.bag)

    def fold(self):
        return [v for v, m in self.bag.items() for _ in range(m)]

    def __repr__(self):
        name = RAGG[self.__class__]
        try:
            v = self.value
        except AggregatorValueError as e:
            v = Term('$error', f'aggregator `{RAGG[self.__class__]}` got bad bag `{e}`')
        return f'`{name} {pp(v)}`'


class BagEquals(BAggr):
    pass


class Shrug(BAggr):

    def __init__(self, vs=None):
        super(Shrug, self).__init__(vs)
        self._val = null_ptr

    def increment(self, v, m):
        self._val = null_ptr
        BAggr.increment(self, v, m)

    def fold(self):

        if self._val == null_ptr:
            vs = [v for v, m in self.bag.items() if m > 0]
            #vs = list(vs)   # for stability
            #vs.sort()
            #return min(vs)
            self._val = np.random.choice(vs)

        return self._val


class ColonEq(BAggr):

    def fold(self):
        [i, v] = max(self.bag)
        vs = {v: m for (r, v), m in self.bag.items() if r == i}
        if sum(vs.values()) == 1:
            return v
        else:
            raise AggregatorValueError(f'`:=` got too many values for rule index `{i}`, got `{list(sorted(vs))}`.')


class AggEquals(BAggr):
    def fold(self):
        if sum(self.bag.values()) > 1:
            raise AggregatorValueError(f'`=` got too many values, got {self.bag}.')
        [v] = set(self.bag)
        return v


# TODO: there may be some sloppiness in the implementation regarding getting
# actual nulls when subtraction was used.
class PlusEquals(BAggr):
    def supports_inverse_elements(self):
        return ALLOW_INVERSE
    def fold(self):
        val = 0.0
        for v, m in self.bag.items():
#            if m < 0: print('+= is using inverse!', val, '+=', v, '@', m)
            #if m == np.inf and v == 0:
            #    # 0*inf needs special handling because inf*0 = nan.
            #    val += 0.0
            #else:
            val += v*m
        return val


# TODO: may be sloppy with nulls.
class TimesEq(BAggr):
    def supports_inverse_elements(self):
        return ALLOW_INVERSE
    def fold(self):
        val = 1.0
        for v, m in self.bag.items():
            #if m == np.inf and v == 0:
            #    val *= 0.0
            #else:
            val *= v**m    # 1**inf = 1
        return val


class MaxEquals(BAggr):
    def fold(self):
        return max(self.bag)


class MinEquals(BAggr):
    def fold(self):
        return min(self.bag)


class OrEquals(BAggr):
    def fold(self):
        assert all(v in [True, False] for v in self.bag)
        return any(self.bag)    # TODO: should really check for bool type.


class AndEquals(BAggr):
    def fold(self):
        assert all(v in [True, False] for v in self.bag)
        return all(self.bag)    # TODO: should really check for bool type.


class SetEquals(BAggr):
    def fold(self):
        return set(self.bag)


class ColonDash(BAggr):
    def fold(self):
        return any(self.bag)


AGG = {
    '+=': PlusEquals,
    '=': AggEquals,
    '->': AggEquals,      # TODO: Not sure I like this.
    'max=': MaxEquals,
    'min=': MinEquals,
    'set=': SetEquals,
    'bag=': BagEquals,
    ':-': ColonDash,
    ':=': ColonEq,
    '*=': TimesEq,
    '&=': AndEquals,
    '|=': OrEquals,
    '?=': Shrug,
}


# Map classes to their names (R+AGG reverse of `AGG`)
RAGG = {v: k for k,v in AGG.items()}


def test():
    agg = PlusEquals()

    agg.increment(3, +1)
    assert agg.value == 3

    agg.increment(3, -1)
    assert agg.value is None

    agg += agg
    assert agg.value is None

    agg2 = PlusEquals()
    agg2.increment(2, 5)
    assert agg2.value == 10

    agg2 += agg
    assert agg2.value == 10

    agg2.increment(4, 5)
    assert agg2.value == 10+20

    agg2.increment(4, -3)
    assert agg2.value == 10+20-12

    from arsenal.assertions import assert_throws
    with assert_throws(AggregatorValueError):
        deref_value('test', MaxEquals({1: -1}))

    print('aggregator tests: pass')


if __name__ == '__main__':
    test()
