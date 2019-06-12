class PlanningFailure(Exception):
    pass

class SolverLimitation(Exception):
    pass

class InstantiationFault(Exception):
    pass

class AggregatorMismatch(Exception):
    pass

class DynaParserException(Exception):
    pass

class ViolatedInvariant(Exception):
    pass

class Fatal(Exception):
    pass

class AggregatorValueError(Exception):
    pass

class Timeout(Exception):
    pass

class DynaCroak(Exception):
    pass
