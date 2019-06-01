from .interpreter import *

class UnkMemo(RBaseType):

    def __init__(self, variables :Tuple[Variable], body_variables :Tuple[Variable], body :RBaseType, supported_modes :Tuple[bool]):
        assert len(variables) == len(body_variables)
        self.variables = variables
        self.body_variables = body_variables or variables
        self.body = body

    @property
    def vars(self):
        return self.variables

    def rename_vars(self, remap):
        return UnkMemo(tuple(remap(v) for v in self.Variable), self.body, self.body_variables)

@simplify.define(UnkMemo)
def simplify_unkmemo(self, frame):
    # the idea should be that if we are handling different modes

    pass
