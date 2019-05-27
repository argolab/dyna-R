from interpreter import *

class Assumptions:
    """
    The assumption object that we are going to track
    This object only supports a single step

    """

    def __init__(self):

        self._dependents = set()
        self._invalid = False

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




class Guard(RBaseType):

    def __init__(self, precondtions, body):
        self._precondition = precondtions
        self._body = body

    @property
    def children(self):
        return self._precondition, self._body


class GuardDispatch(RBaseType):

    def __init__(self, children :List[RBaseType]):
        self._children = self.

@simplify.define(GuardDispatch)
def simplify_guard
