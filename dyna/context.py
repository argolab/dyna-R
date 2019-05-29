
from .interpreter import *
from .terms import CallTerm

class SystemContext:
    """
    Represents the dyna system with the overrides for which expressions are going to be set and written
    """

    def __init__(self, parent=None):
        # the terms as the user defined them (before we do any rewriting) we can
        # not delete these, as we must keep around the origional definitions
        # so that we can recover in the case of "delete everything" etc
        self.terms_as_defined = {}

        # if there is some rewriting process that we have for terms, then we need to determine when these expressions are changing
        self.terms_as_rewritten = {}

        self.agenda = None

        # where we fallback for pther defined
        self.parent = None

    def define_term(self, name, arity, rexpr):
        assert (name, arity) not in self.terms_as_defined
        self.terms_as_defined[(name, arity)] = rexpr

    def add_to_term(self, name, arity, rexpr):
        # check that the aggregator is the same and the combine the expressions
        # together anything that depends on the value of the expression will
        # need to be invalided.
        assert False

    def define_infered(self, required :RBaseType, added :RBaseType):
        pass

    def call_term(self, name, arity) -> RBaseType:
        # this should return a method call to a given term.
        # this should be lazy?  So that
        return CallTerm(name, variables_named('Ret', *range(arity)), self, (name, arity))

    def lookup_term(self, name):
        # if this isn't defined, then we can delay, or report an error to the
        # user that they are trying to use a method that isn't defined.

        if name in self.terms_as_rewritten:
            return self.terms_as_rewritten[name]
        if name in self.terms_as_defined:
            return self.terms_as_defined[name]
        if self.parent:
            return parent.lookup_term(name)


# where we will define the builtins etc the base dyna base, for now there will
# just be a single one of these that is global however we should not use the
# global reference whenever possible, as will want to turn this into the
# dynabase references
dyna_system = SystemContext()

class TaskContext:
    """
    Represent the context for a given runtime transaction.
    This is going to be some task that pops of the agenda.
    The reason for this class is basically we might want to mask some memo table with new updates,
    or track what operations would need to be pushed to the agenda.
    The infinite priority agenda should basically be "pushing" something to this local agenda,
    but then choosing to run it /instantly/ at this point, and then getting the result instead of using whatever the /guessed/ was going to be
    """
    def __init__(self, system):
        self.new_memos = {}
        self.agenda_additions = []
        self.system = system
