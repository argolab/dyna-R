from .interpreter import *

class Assumption:
    """
    The assumption object that we are going to track
    This object only supports a single step

    """

    def __init__(self):
        self._dependents = set()
        self._invalid = False  # invalid can only go from False -> True, there is no transition back to False

    def track(self, reciever):
        assert not self._invalid
        self._dependents.add(reciever)

    def invalidate(self):
        self._invalid = True
        for d in self._dependents:
            d.notify_invalidated()

    def notify_invalidated(self):
        # notify that the assumption is invalidated
        self.invalidate()

    def notify(self):
        # this should be overriden such that it tracks
        self.invalidate()

    def signal(self, msg):
        for d in self._dependents:
            d.signal()


class AssumptionWrapper(RBaseType):

    def __init__(self, assumption :Assumption, body :RBaseType):
        self.assumption = assumption
        self.body = body

    @property
    def children(self):
        return self.body,

    def rewrite(self, rewriter):
        return AssumptionWrapper(self.assumption, rewriter(self.body))

@simplify.define(AssumptionWrapper)
def simplify_assumption(self, frame):
    # this doesn't do anything, as something else is going to have to get all of the assumptions
    return simplify(self.body, frame)

get_all_assumptiosn = Visitor()

@get_all_assumptiosn.default
def get_assumptions_default(self):
    for c in self.children:
        yield from get_all_assumptiosn(c)

@get_all_assumptiosn.define(AssumptionWrapper)
def get_assumptions_wrapper(self):
    yield self.assumption
    yield from get_all_assumptiosn(self.body)




class Guard(RBaseType):

    def __init__(self, precondtions, body):
        assert False # TODO
        self._precondition = precondtions
        self._body = body

    @property
    def children(self):
        return self._precondition, self._body


class GuardDispatch(RBaseType):

    def __init__(self, children :List[RBaseType]):
        assert False  # TODO
        self._children = children

@simplify.define(GuardDispatch)
def simplify_guard(self, frame):
    pass
