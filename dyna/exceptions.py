class DynaSolverError(RuntimeError):
    def __init__(self, msg=None):
        super().__init__(msg)


class DynaSolverErrorSuggestPrompt(DynaSolverError):

    def __init__(self, msg, suggest_prompt, suggest_api):
        super().__init__(msg)
        self.suggest = suggest
        self.suggest_api = suggest_api

    def __repr__(self):
        return f'{self.msg}\n{self.suggest_api}'


class DynaSolverUnLoopable(DynaSolverError):
    """In the case that the program can not run the aggregator even though all of
    the arguments are ground This should suggest that something is memoized.
    Though telling exactly what can be a bit complicated.

    Might want to suggest the different calls that are present in the expresion
    that is being looped over.  In the case that something is recursive, then it would

    """

    def __init__(self, R):
        super().__init__()
        self.Rexpr = R

class DynaSolverGussed(DynaSolverError):
    """
    In the case that the solver needs to make a guess and start forward chaining in the case of a cycle
    """
    pass


class DynaSolverProcessAgendaAndRestart(DynaSolverError):
    """
    Make the system stop the current query, run the agenda until converged, and then this can restart the current query
    """
    pass



class DivergedValue(object):
    """Represent an infinite aggregation where the result would diverge or be
    illdefined?  Multiplication by zero could return zero, otherwise this should
    just return itself.  Basically like a version of inf?
    """

    pass
