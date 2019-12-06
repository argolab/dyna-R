class DynaSolverError(RuntimeError):
    def __init__(self, msg):
        super().__init__(msg)


class DynaSolverErrorSuggestPrompt(DynaSolverError):

    def __init__(self, msg, suggest):
        super().__init__(msg)
        self.suggest = suggest
