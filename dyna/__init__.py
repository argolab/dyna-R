from . import interpreter
from . import builtins
from . import terms
from . import context
from . import guards

from .interpreter import (
    RBaseType, FinalState, Terminal, Variable, variables_named, constant, Frame,
    simplify, getPartitions, saturate, loop,
    intersect as Intersect, partition as Partition, Unify, Aggregator, AggregatorOpImpl, AggregatorOpBase
)

from .terms import (
    Term, BuildStructure, ReflectStructure, Evaluate
)

from .memos import MemoContainer, RMemo, naive_converge_memos, rewrite_to_memoize

from .optimize import run_optimizer

# This is a shortcut for writing code quickly where we are going to lookup a method
# so we can write something like `M.add(1,2,3)`
class M(object):
    # TODO?: maybe shouldn't have this return the resulting variable, it feels
    # that from unit tests that it is easier to specify the return variable.
    def __getattribute__(self, n):
        def f(*args):
            ret = interpreter.VariableId(('Ret', object()))
            r = context.dyna_system.lookup_term((n, len(args)))
            return r(*args, ret=ret), ret
        return f
M = M()
