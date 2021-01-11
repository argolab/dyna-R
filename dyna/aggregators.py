from .interpreter import *
from .terms import Term
from collections import Counter

class AggregatorEqual(AggregatorOpBase):
    selective = True
    def lift(self, x): return x
    def lower(self, x): return x
    def combine(self, a,b):
        return Term('$error', ("Aggregator `=` should not have more than one value",))

class AggregatorSaturate(AggregatorOpBase):
    selective = True
    def __init__(self, op, saturated):
        self.op = op
        self.saturated = saturated

    def lift(self, x):
        if self.saturated == x:
            raise AggregatorSaturated(x)
        return x
    def lower(self, x): return x
    def combine(self, a,b):
        r = self.op(a,b)
        if self.saturated == r:
            # this should identify that this operation is done, or saturated
            raise AggregatorSaturated(r)
        return r

null_term = Term('$null', ())
class AggregatorColonEquals(AggregatorOpBase):
    selective = True
    def lift(self, x): return x
    def lower(self, x):
        assert x.name == '$colon_line_tracking'
        r = x.get_argument(1)
        if r == null_term:
            return None
        return r
    def combine(self, a,b):
        assert a.name == '$colon_line_tracking'
        assert b.name == '$colon_line_tracking'
        if a.get_argument(0) > b.get_argument(0):
            return a
        else:
            return b

_colon_line_tracking = -1
def colon_line_tracking():
    global _colon_line_tracking
    _colon_line_tracking += 1
    return _colon_line_tracking

def gc_colon_equals(rexpr):
    assert isinstance(rexpr, Aggregator)
    assert rexpr.aggregator is AGGREGATORS[':=']

    partition = rexpr.body
    assert isinstance(partition, Partiton)

    raise NotImplementedError()  # TODO finish


class AggregatorSetEquals(AggregatorOpBase):
    def lift(self, x): return {x}
    def lower(self, x):
        l = list(x)
        l.sort()
        return Term.fromlist(l)
    def combine(self, a,b):
        return a | b

class AggregatorBagEquals(AggregatorOpBase):
    def lift(self, x):
        return Counter({x: 1})
    def lower(self, x):
        l = [Term('$', (a,b)) for a,b in x.items()]
        l.sort(key=lambda x: (-x.get_argument(1), x.get_argument(0)))  # sort by the largest count first and then the value
        return Term.fromlist(l)
    def combine(self, a,b):
        return a + b

class AggregatorQuestionMark(AggregatorOpBase):
    # this isn't going to be stable.  The system really needs to know that it should be forced to memoize the result of these expressions
    # but that is going to have to be managed elsewhere? something that will detect that it is using this aggregator, and then tag that it must
    # be memoized or something
    def lift(self, x): return x
    def lower(self, x): return x
    def combine(self, a,b): return a

# the colon equals aggregator needs to be able to identify if there is a value which is partially instantiated
# in the case that something else is added?
#
# There could be something which adds additional R-exprs to unpack the aggregated value and then compares which line
# number is in use.
#
# Currently, the aggregator on "runs" in the case that all of the keys are bound.  This would need



AGGREGATORS = {
    '=': AggregatorEqual(),
    '+=': AggregatorOpImpl(lambda a,b: a+b),
    '*=': AggregatorOpImpl(lambda a,b: a*b),
    'max=': AggregatorOpImpl(max, True),
    'min=': AggregatorOpImpl(min, True),
    ':-': AggregatorSaturate(lambda a,b: a or b, True),
    '|=': AggregatorSaturate(lambda a,b: a or b, True),
    '&=': AggregatorSaturate(lambda a,b: a and b, False),
    ':=': AggregatorColonEquals(),
    'set=': AggregatorSetEquals(),
    'bag=': AggregatorBagEquals(),
    '?=': AggregatorQuestionMark(),
}

removable_aggregators = {
    # there might be an issue with removing max/min.

    AGGREGATORS['max='],
    AGGREGATORS['min='],
    AGGREGATORS[':-'],
    # := and = should also be removable, though maybe depending on how we have defined errors to get handled by those aggregators, it might
}
