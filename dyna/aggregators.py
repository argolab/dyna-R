from .interpreter import *
from .terms import Term

class AggregatorEqual(AggregatorOpBase):
    def lift(self, x): return x
    def lower(self, x): return x
    def combine(self, a,b):
        return Term('$error', ("Aggregator `=` should not have more than two values",))

class AggregatorSaturate(AggregatorOpBase):
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
    def lift(self, x): return x
    def lower(self, x):
        assert x.name == '$colon_line_tracking'
        r = x.arguments[1]
        if r == null_term:
            return None
        return r
    def combine(self, a,b):
        assert a.name == '$colon_line_tracking'
        assert b.name == '$colon_line_tracking'
        if a.arguments[0] > b.arguments[0]:
            return a
        else:
            return b


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
    'max=': AggregatorOpImpl(max),
    'min=': AggregatorOpImpl(min),
    ':-': AggregatorSaturate(lambda a,b: a or b, True),
    '|=': AggregatorSaturate(lambda a,b: a or b, True),
    '&=': AggregatorSaturate(lambda a,b: a and b, False),
    ':=': AggregatorColonEquals()
}
