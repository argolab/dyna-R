from . import interpreter
from . import builtins

from .interpreter import (
    RBaseType, FinalState, Terminal, Variable, variables_named, constant, Frame,
    simplify, getPartitions,
    intersect as Intersect, partition as Partition, Unify, Aggregator, AggregatorOpImpl, AggregatorOpBase
)

# This is a shortcut for writing code quickly where we are going to lookup a method
# so we can write something like `M.add(1,2,3)`
class M(object):
    def __getattribute__(self, n):
        def f(*args):
            r = context.dyna_system.lookup_term((n, len(args)))
            # TODO: this needs to apply arguments to the returned R-expr and return that
            if r is not None:
                args = interpreter.variables_named(*args)
                ret = interpreter.VariableId(('Ret', object()))  # we need a unique name for the variable that holds the return value
                renames = {
                    interpreter.ret_variable: ret
                }
                for i, a in enumerate(args):
                    renames[interpreter.VariableId(i)] = a
                for v in r.all_vars():
                    if v not in renames and not isinstance(v, interpreter.ConstantVariable):
                        renames[v] = interpreter.VariableId()

                return r.rename_vars(lambda x: renames.get(x,x)), ret
        return f
M = M()
