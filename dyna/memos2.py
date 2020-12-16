from .interpreter import *


class PriorityUnion(RBaseType):

    def __init__(self, var_map, indicator, memos, base_rexpr):
        self.var_map = var_map
        self.indicator = indicator
        self.memos = memos
        self.base_rexpr = base_rexpr

    def rename_vars(self, remap):
        # if none of the variables are renamed, then this could just stay the same
        return PriorityUnion(
            {k: remap(v) for k,v in self.var_map},
            self.indicator,
            self.memos,
            self.base_rexpr
        )

    def rewrite(self, rewriter):
        # this should avoid getting rewritten?  It would have that the memos are only contained
        return self


    @property
    def vars(self):
        return tuple(self.var_map.values())

    def _tuple_rep(self):
        return self.__class__.__name__, self.variables, self.indicator, self.memos, self.base_rexpr

@simplify.define(PriorityUnion)
def simplify_priorityUnion(self, frame):
    # this should identify if it is in a mode which
    frame_keys = Frame()
    for k,v in self.var_map.items():
        if v.isBound(frame):
            k.setValue(frame_keys, v.getValue(frame))
    Q = simplify(self.indicator, frame_keys)
    state = None
    if isinstance(Q, Terminal):
        if Q.multiplicity >= 1:
            state = 'lookup'
        else:
            assert Q.multiplicity == 0
            state = 'compute'
    elif isinstance(Q, Partition):
        # this is looking for osmething like M + Q, where the value of M would
        for key, values in self._children.items():
            for v in values:
                if isinstance(v, Terminal) and v.multiplicity > 0:
                    # then this just needs to check if the key is something that matches for the cases which are left
                    # otherwise this is going to



class PartitionProgram(RBaseType):
    # this is `is(Key,Val)`

    def __init__(self, program, key_var, val_var):
        super().__init__()
        assert isinstance(program, dict)
        self.program = program
        # self.key_var = key_var
        # self.val_var = val_var

    def rename_vars(self, remap):
        return PartitionProgram(self.program, remap(self.key_var), remap(self.val_var))

    @property
    def vars(self):
        return self.key_var, self.val_var

    # @property
    # def children(self):
    #     return ()  # do not return the recursive version of the program

    # def rewrite(self, rewriter):
    #     return self

    def _tuple_rep(self):
        return self.__class__.__name__, self.key_var, self.val_var

@simplify.define(PartitionProgram)
def simplify_partition_program(self, frame):
    return self  # this is a NOP
    # if self.key_var.isBound(frame):
    #     pass
    # assert False



class CallProgramTerm(RBaseType):

    def __init__(self, var_map, term_ref):
        super().__init__()
        self.var_map = var_map
        self.term_ref = term_ref

    @property
    def vars(self):
        return tuple(self.var_map.values())

    def rename_vars(self, remap):
        return CallProgramTerm({k: remap(v) for k,v in self.var_map.items()}, term_ref)



@simplify.define(CallProgramTerm)
def simplify_callprogramterm(self, frame):
    program_handle = frame.program_handle
    if program_handle is not None:
        rexpr = program_handle.program.get(self.term_ref)
        if rexpr is None:
            return Terminal(0)
        else:
            return rexpr.rename_vars_unique(self.var_map.get)
    return self


def priority_union(a,b):
    if isinstance(a, Terminal):

    if isinstance(a, TerminalBlock):
        return Terminal(0)
    return



class PriorityUnion(RBaseType):

    def __init__(self, higher_priorty, lower_priority):
        super().__init__()
        self.higher_priorty = higher_priorty
        self.lower_priority = lower_priority

    def rewrite(self, rewriter):
        return PriorityUnion


class PriorityUnionZeroValue:

    def __add__(self, other):
        if isinstance(other, int):
            if other == 0:
                return self
            return other
        elif isinstance(other, PriorityUnionZeroValue):
            return self
        assert False

    def __mul__(self, other):
        if isinstance(other, int):
            if other == 0:
                return 0
            return self
        elif isinstance(other, PriorityUnionZeroValue):
            return self
        assert False

    def __eq__(self, other):
        return isinstance(other, PriorityUnionZeroValue) or other == 0

    def __hash__(self): return 0
    def __str__(self): return '(PRIORITY_UNION_ZERO)'
    def __repr__(self): return '(PRIORITY_UNION_ZERO)'

class PriorityUnionZeroR(Terminal):

    def __init__(self):
        super().__init__(PriorityUnionZeroValue())
    def isEmpty(self):
        return False
