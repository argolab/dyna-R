# represent parameters which can be updated during each epoch This allows for
# something to implement SGD on top of dyna rather than having to perform time
# stepping



from .context import dyna_system
from .builtins import moded_op
from .interpreter import simplify, Frame


class SteppableParamters(object):

    def __init__(self):
        self.collections = {}  # map of (Name, arity) -> memo tables which contain what expressions should be returned

        self.parameters = None  # this should be some RMemo table representation
        self.updates = None

    def step(self):
        # this should take any values which are present in the updates and then
        # apply them to the parameters.  In the case that there are new values which are present

        # this needs to be moded as a partition, otherwise this is not going to work well?
        nP = simplify(self.parameters, Frame(), flatten_keys=True)


    def get_collection(self, name, arity):
        assert False



class SteppableParametersAccess(RBaseType):

    def __init__(self, parameter_collection, name_var, arity_var, arg_var, result_var):
        self.parameter_collection = parameter_collection
        self.name_var = name_var
        self.arity_var = arity_var
        self.arg_var = arg_var
        self.result_var = result_var

    @property
    def vars(self):
        return self.name_var, self.arity_var, self.arg_var, self.result_var

    def rename_vars(self, remap):
        return SteppableParametersAccess(self.parameter_collection, remap(self.name_var), remap(self.arity_var), remap(self.arg_var), remap(self.result_var))

@simplify.define(SteppableParametersAccess)
def simplify_parameters_acess(self, frame):
    if self.name_var.isBound(frame) and self.arity_var.isBound(frame):
        # then we should look up the corresponding memoized expression and

        # if there is nothing in the collection, then it should identify that it wants to

        assert False
    return self  # can not load the parameters yet



# def get_parameters(name, key, default):
#     # this needs to provide some lookup operation where it can return a R-expr
#     # which performs access operations.  Once the updates have been applied,
#     # this should push new agenda entries.
#     pass



def define_parameter_operations(dyna_system):

    parameter_collection = SteppableParamters()

    dyna_system.define_term('$__parameters_current', 3,
                            SteppableParametersAccess(parameter_collection,
                                                      VariableId(0), VariableId(1), VariableId(2), interpreter.ret_variable))

    dyna_system.agenda._agenda_empty_notfies.append(parameter_collection.step)

# the update should be applied only in the case that the agenda has complete drained.
# which can then fire off more updates downstream to other operations


# this should just return $null in the case that there is nothing which is currently stored
# this could be used with something like:
#    value(X) := random(*).
#    value(X) := $parameters(X).
# which would allow for it to be inited with a random value.

# to update the value would have something like:
# $parameters_next(X) := $parameters(X).
# $parameters_next(X) := value(X) + $gradient(&'$loss', X).
# then this is basically just taking anything which has been set to $parameters_next and makes it so
# parameters will take the value.
#
# there only needs to be a single argument?  As higher order structure can just
# be represented as having a named type.  Thsis less efficient given the current
# implementation, as it would have to pattern match against some data table
# which has different representations to parsel out which values match.  Though
# this could just be time stepping the values directly? Or have that there are
# different tables which store a given value.
#
# it is possible to use the reflection operation to partition this into
# different tables, so this might not be that bad?  There can just be some
# underlying structure
# '$__parameters_next'(Name, A1, X) = $reflect(X, Name, [A1|_]), $parameters_next(X).
# '$parameters(X) = $reflect(X, Name, [A1|_]), $__parameters_current(Name, A1, X).
#
# optimize as currently written would not remove the reflect operation, as it
# would require identifying that it can push through the partition which
# represents the parameters.  This would have to lift the operation higher?
# Something like a merge partition, where it introduces extra variables to an expression
