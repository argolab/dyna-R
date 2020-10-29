# represent parameters which can be updated during each epoch This allows for
# something to implement SGD on top of dyna rather than having to perform time
# stepping


from .interpreter import *
from .builtins import moded_op
from .terms import BuildStructure
from .memos import MemoContainer, RMemo, AgendaMessage
from .prefix_trie import zip_tries

PARAMETERS_NAME_FORMAT = '$__parameters_values_{name}/{arity}'
PARAMETERS_NEXT_FORMAT = '$__parameters_next_{name}/{arity}'

class SteppableParamters(object):

    def __init__(self, dyna_system):
        self.collections = {}  # map of (Name, arity) -> memo tables which contain what expressions should be returned
        self.dyna_system = dyna_system
        self.stepping_enabled = True

    def watch_do_step_callback(self, msg):
        self.stepping_enabled ,  = msg.key

    def step(self):
        # this should take any values which are present in the updates and then
        # apply them to the parameters.  In the case that there are new values which are present

        # this needs to be moded as a partition, otherwise this is not going to work well?
        #nP = simplify(self.parameters, Frame(), flatten_keys=True)

        if not self.stepping_enabled:
            # do nothing in this case
            return

        # double check the result
        do_step = self.dyna_system.raw_call('$parameters_step', ())
        if do_step is False:
            return

        for (name, arity), (source, dest) in self.collections.items():
            # identify differences in the source and destination memo
            # this is modeled after refresh_whole_table

            dm = dest.memos._children
            dassum = dest.assumption

            changes = []
            for key, a, b in zip_tries(dm, source.memos._children):
                if a != b:
                    changes.append((key, b))

            for key, value in changes:
                if value is None:
                    del dm[key]
                else:
                    dm[key] = value

                mm = AgendaMessage(table=dest, key=key, is_null_memo=True)
                dassum.signal(mm)


    def ensure_collection(self, name, arity):
        if (name, arity) not in self.collections:
            self.collections[(name, arity)] = None
            args = [VariableId(i) for i in range(arity)]
            memo_args = args + [ret_variable]

            dest_memos = MemoContainer((True,)*arity+(False,), (False,)*(arity+1), memo_args,
                                       Partition(tuple(memo_args), PrefixTrie(arity+1)),
                                       is_null_memo=True, dyna_system=self.dyna_system)

            # this needs to define a new term which will be updated with the values later
            # the later invalidated value will be with a memoization container which
            self.dyna_system.define_term(PARAMETERS_NAME_FORMAT.format(name=name,arity=arity), arity, RMemo(memo_args, dest_memos))

            key_var = VariableId()
            next_parameters = self.dyna_system.call_term('$__parameters_next', 3)
            next_parameters = next_parameters(constant(name), constant(arity), key_var, ret=ret_variable)
            r = intersect(next_parameters, BuildStructure(name, key_var, args))
            self.dyna_system.define_term(PARAMETERS_NEXT_FORMAT.format(name=name,arity=arity), arity, r)
            self.dyna_system.optimize_term((PARAMETERS_NEXT_FORMAT.format(name=name,arity=arity), arity))
            # make this memoized, so that we can just copy the memo table later
            source_R = partition(tuple(memo_args),
                                 [self.dyna_system.call_term(PARAMETERS_NEXT_FORMAT.format(name=name,arity=arity), arity)])

            source_memos = MemoContainer((True,)*arity+(False,), (False,)*(arity+1), memo_args,
                                         source_R, is_null_memo=True, dyna_system=self.dyna_system)

            self.collections[(name,arity)] = (source_memos, dest_memos)


class SteppableParametersAccess(RBaseType):

    def __init__(self, parameter_collection, name_var, arity_var, arg_var, result_var):
        super().__init__()
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

    def _tuple_rep(self):
        return self.__class__.__name__, self.name_var, self.arity_var, self.arg_var, self.result_var

@simplify.define(SteppableParametersAccess)
def simplify_parameters_access(self, frame):
    if self.name_var.isBound(frame) and self.arity_var.isBound(frame):
        # then we should look up the corresponding memoized expression and

        # if there is nothing in the collection, then it should identify that it wants to

        name = self.name_var.getValue(frame)
        arity = self.arity_var.getValue(frame)

        if not isinstance(name, str) or not isinstance(arity, int):
            return Terminal(0)

        self.parameter_collection.ensure_collection(name, arity)

        arg_vars = tuple(VariableId() for _ in range(arity))
        match_structure = BuildStructure(name, self.arg_var, arg_vars)

        call = self.parameter_collection.dyna_system.call_term(PARAMETERS_NAME_FORMAT.format(name=name, arity=arity), arity)
        call = call(*arg_vars, ret=self.result_var)

        return intersect(match_structure, call)

    return self  # can not load the parameters yet


def define_parameter_operations(dyna_system):

    parameter_collection = SteppableParamters(dyna_system)

    dyna_system.define_term('$__parameters_current', 3,
                            SteppableParametersAccess(parameter_collection,
                                                      VariableId(0), VariableId(1), VariableId(2), ret_variable))

    dyna_system.agenda._agenda_empty_notfies.append(parameter_collection.step)

    dyna_system.add_rules("""
    $parameters(X) = $reflect(X, Name, Arity, _), '$__parameters_current'(Name, Arity, X).
    '$__parameters_next'(Name, Arity, X) = $reflect(X, Name, Arity, _), $parameters_next(X).

    % not using := here as the order in which this rule loads in comparision to
    % the other code in the program is not 100% clear at times....
    $parameters_step &= true.
    """)

    # push the optimizer to run on this expression so that it will remove the reflect from
    # the expression which makes it easier to processes
    dyna_system.optimize_term(('$__parameters_next', 3))

    dyna_system.watch_term_changes(('$parameters_step', 0), parameter_collection.watch_do_step_callback)


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
