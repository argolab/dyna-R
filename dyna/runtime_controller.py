"""This file contains a higher level controller for the runtime.  Anytime that
something happens that we might be able to control and select between different
opions for, this should get called with the approperate method

"""

from .exceptions import DynaSolverUnLoopable
from collections import deque

class RuntimeController(object):

    def do_simplify_rewrite(self, R, frame):
        raise NotImplementedError()

    def conjunctive_rewrite_order(self, R :'RBaseType', frame :'Frame', conjuncts :'list[RBaseType]'):
        raise NotImplementedError()

    def disjunctive_rewrite_order(self, R :'RBaseType', frame :'Frame', disjuncts :'list[RBaseType]'):
        raise NotImplementedError()

    def select_loop_variable(self, R :'RBaseType', frame :'Frame', loopable :'list[Variable]'):
        raise NotImplementedError()

    def agenda_pop(self, agenda):
        raise NotImplementedError()

    def agenda_push(self, agenda):
        raise NotImplementedError()

    def agenda_new(self):
        raise NotImplementedError()

    def term_defined(self, name, R):
        raise NotImplementedError()

    def unloopable(self, R):
        raise NotImplementedError()


class NOOPRuntimeController(RuntimeController):

    def do_simplify_rewrite(self, R, frame):
        return True

    def conjunctive_rewrite_order(self, R, frame, conjuncts):
        return conjuncts

    def disjunctive_rewrite_order(self, R, frame, disjuncts):
        return disjuncts

    def select_loop_variable(self, R, frame, loopable):
        for p in loopable:
            return p

    def agenda_pop(self, agenda :deque):
        return agenda.popleft()

    def agenda_push(self, agenda :deque, task):
        agenda.append(task)

    def agenda_new(self):
        return deque()

    def term_defined(self, name, R):
        return R

    def unloopable(self, R):
        # this could look at the stack track to try and identify something that
        # could be memoized.  if something loops multiple times, then it would
        # be a candidate for which this might want to memoize some expression
        raise DynaSolverUnLoopable(R)



# global value which could be called anytime that there are values which
controller = NOOPRuntimeController()


from .interpreter import RBaseType, Frame, Variable  # needed for the type annotations