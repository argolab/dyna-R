from .exceptions import *
from . import interpreter
from . import builtins
from . import terms
from . import context
from . import guards
from . import aggregators

# dyna_system = context.dyna_system # this will get moved into this file as the default

from .interpreter import (
    RBaseType, FinalState, Terminal, Variable, variables_named, constant, Frame,
    simplify, getPartitions, saturate, loop,
    intersect as Intersect, partition as Partition, Unify, Aggregator, AggregatorOpImpl, AggregatorOpBase
)

from .terms import (
    Term, BuildStructure, ReflectStructure, Evaluate, Evaluate_reflect
)

from .memos import MemoContainer, RMemo, naive_converge_memos, rewrite_to_memoize

from .optimize import run_optimizer

from . import compiler

# extra optimizations
from . import builtin_matrix_ops


dyna_system = context.SystemContext()


# load the prelude file
import os as _os
with open(_os.path.join(_os.path.dirname(__file__), 'prelude.dyna'), 'r') as _f:
    from dyna.syntax.normalizer import add_rules as _add_rules
    _add_rules(_f.read())
