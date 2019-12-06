from .interpreter import *

class Assumption:
    """
    The assumption object that we are going to track
    This object only supports a single step

    """

    def __init__(self, name=None):
        self._dependents = set()
        self._invalid = False  # invalid can only go from False -> True, there is no transition back to False
        self._name = name

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
            d.signal(msg)

    def __str__(self):
        return f'Assumption({self._name}, valid={not self._invalid})'

    def __repr__(self):
        return str(self)

    def isValid(self):
        return not self._invalid

class AssumptionListener:
    # something that can be invalidated, and then will turn of getting further
    # signals, though, we are going to want to clean these out or something....

    def __init__(self, wrapped):
        self.wrapped = wrapped
        self._invalid = False

    def invalidate(self):
        i = self._invalid
        self._invalid = True
        if not i:
            self.wrapped.invalidate()

    def signal(self, msg):
        if not self._invalid:
            self.wrapped.signal(msg)

    def notify_invalidated(self):
        # TODO: is this a significant difference in this case?  Is there
        # something else that should be done here
        self.invalidate()

class AssumptionResponse:

    def __init__(self, method):
        self.method = method

    def signal(self, msg):
        raise NotImplementedError()

    def invalidate(self):
        if self.method is not None:
            r = self.method
            self.method = None
            r()

    def notify_invalidated(self):
        self.invalidate()



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
    assert self.assumption.isValid()
    frame.assumption_tracker(self.assumption)
    return simplify(self.body, frame)

get_all_assumptions = Visitor(track_source=False)

@get_all_assumptions.default
def get_assumptions_default(self):
    for c in self.children:
        yield from get_all_assumptions(c)

@get_all_assumptions.define(AssumptionWrapper)
def get_assumptions_wrapper(self):
    yield self.assumption
    yield from get_all_assumptions(self.body)


remove_all_assumptions = Visitor(track_source=False)

@remove_all_assumptions.define(AssumptionWrapper)
def remove_all_assumptions_assumption(self, track=None):
    if track is not None:
        track(self.assumption)
    return remove_all_assumptions(self.body, track)




# class Guard(RBaseType):

#     def __init__(self, precondtions, body):
#         assert False # TODO
#         self._precondition = precondtions
#         self._body = body

#     @property
#     def children(self):
#         return self._precondition, self._body


# class GuardDispatch(RBaseType):

#     def __init__(self, children :List[RBaseType]):
#         assert False  # TODO
#         self._children = children

# @simplify.define(GuardDispatch)
# def simplify_guard(self, frame):
#     pass
