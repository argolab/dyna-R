
# this file should probably be renamed to something like dynabase or something,
# it is holding the references to the different terms that are defined.



# maybe these should be imported later or not at all?
from .interpreter import *
from .terms import CallTerm, Evaluate, Evaluate_reflect
from .guards import Assumption, AssumptionWrapper, AssumptionResponse
from .agenda import Agenda
from .optimize import run_optimizer
from .compiler import run_compiler

from functools import reduce

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
        self.terms_as_optimized = {}

        # the memo tables that are wrapped around the terms.
        self.terms_as_memoized = {}

        # Dict[RExpr, CompiledRexprs]
        # when we compile a term this will be the resulting reference for that object
        self.terms_as_compiled = {}

        self.merged_expressions = {}

        self.term_assumptions = {}

        self.agenda = Agenda()

        self.infered_constraints = []  # going to want some matching expression against having multiple
        self.infered_constraints_index = {}

        # where we fallback for other defined expressions
        self.parent = None

    def term_assumption(self, name):
        if name not in self.term_assumptions:
            self.term_assumptions[name] = Assumption(name)
        return self.term_assumptions[name]

    def invalidate_term_assumption(self, name):
        a = self.term_assumption(name)
        n = Assumption(name)
        self.term_assumptions[name] = n
        a.invalidate()
        return n

    def delete_term(self, name, arity):
        a = (name, arity)
        if a in self.terms_as_defined:
            del self.terms_as_defined[a]
        if a in self.terms_as_optimized:
            del self.terms_as_optimized[a]
        if a in self.terms_as_memoized:
            del self.terms_as_memoized[a]

        # do invalidation last as we want anything that rechecks to get the new values
        self.invalidate_term_assumption(a)
        # if there is something in the parent, then maybe we should instead save
        # this as empty, that way we can track that we are resetting.  Though
        # maybe if we have fully overwritten something, then we are going to
        # track that?

    def define_term(self, name, arity, rexpr):
        assert (name, arity) not in self.terms_as_defined
        self.terms_as_defined[(name, arity)] = rexpr
        self.invalidate_term_assumption((name, arity))

    def add_to_term(self, name, arity, rexpr):
        # check that the aggregator is the same and the combine the expressions
        # together anything that depends on the value of the expression will
        # need to be invalided.
        a = (name, arity)
        if a not in self.terms_as_defined:
            self.terms_as_defined[a] = rexpr
        else:
            # then we need to combine these expressions together
            prev = self.terms_as_defined[a]
            # there should be an aggregator on the outer level, so we first check that, and it doesn't match then we are going to raise an error
            if not isinstance(prev, Aggregator) or not isinstance(rexpr, Aggregator) or prev.aggregator != rexpr.aggregator:
                raise RuntimeError("mismatch aggregator")
            # we are giong to rewrite the new expression to match the current expression
            nm = {}
            nm.update(dict(zip(rexpr.head_vars, prev.head_vars)))
            nm[rexpr.result] = prev.result
            nm[rexpr.body_res] = prev.body_res

            nr = rexpr.rename_vars(lambda x: nm.get(x,x))

            # merge the branches of the partition
            assert isinstance(prev.body, Partition) and isinstance(nr.body, Partition)
            assert prev.body._unioned_vars == nr.body._unioned_vars  # check that the orders are the same, otherwise this would require a more complicated merge

            # this is going to modify the branches of the currently stored body rather than create something new...sigh, I guess we also do this with the memos

            mt = prev.body._children
            for key, vals in nr.body._children.items():
                mt.setdefault(key, []).extend(vals)

        # track that this expression has changed, which can cause things to get recomputed/propagated to the agenda etc
        self.invalidate_term_assumption(a)

    def define_infered(self, required :RBaseType, added :RBaseType):
        z = (required, added)
        self.infered_constraints.append(z)

        # this is the constraint with the most number of variables attached, so
        # the thing that we are going to look for an index on
        ri = max(required.all_children(), key=lambda x: len(x.vars))
        self.infered_constraints_index.setdefault(type(ri), {}).setdefault(ri.weak_equiv()[0], []).append(z)


    def call_term(self, name, arity) -> RBaseType:
        # this should return a method call to a given term.
        # this should be lazy.

        m = {x:x for x in variables_named(*range(arity))}
        m[ret_variable] = ret_variable
        return CallTerm(m, self, (name, arity))

    def lookup_term(self, name, ignore=()):
        # if a term isn't defined, we are going to return Terminal(0) as there
        # is nothing that could have unified with the given expression.  we do
        # included tracking with the assumption, so if it later defined, we are
        # able to change the expression.

        if name in self.terms_as_memoized and 'memo' not in ignore:
            r = self.terms_as_memoized[name]  # should contain an RMemo type which will perform reads from a memo table
        elif name in self.terms_as_compiled and 'compile' not in ignore:
            r = self.terms_as_compiled[name]
        elif name in self.terms_as_optimized and 'optimized' not in ignore:
            r = self.terms_as_optimized[name]  # this is the term rewritten after having been passed through the optimizer
        elif name in self.terms_as_defined:
            r = self.terms_as_defined[name]  # something that was defined directly by the user
        elif isinstance(name, MergedExpression) and name in self.merged_expressions:
            r = name.expression  # the optimizer has combined some states together, but we have not processed this item yet, so we can just return the expression as it will be semantically equivalent
        elif self.parent:
            r = self.parent.lookup_term(name)
        else:
            assert not isinstance(name, (MergedExpression,CompiledExpression))  # this should get handled at some point along the chain. So it should not get to this undefiend point
            r = Terminal(0)  # this should probably be an error or something so that we can identify that this method doesn't exit

        # wrapped the returned result in an assumption so we can track if the
        # code changes.
        return AssumptionWrapper(self.term_assumption(name), r)

    def run_agenda(self):
        return self.agenda.run()

    def optimize_system(self):
        # want to optimize all of the rules in the program, which will then
        # require that expressions are handled if they are later invalidated?

        for term, rexpr in self.terms_as_defined.items():
            # this should call optimize on everything?

            b = check_basecases(rexpr)
            if b != 3:
                # then we want to try and improve this expression as there is
                # something that we can try and optimize.
                self.agenda.push(lambda: self._optimize_term(term))

    def create_merged_expression(self, expr :RBaseType, exposed_vars: Set[Variable]):
        # if there are some terms that are combiend, then we want to be made
        # aware of that, so that we can plan optimizations on the new inferred
        # terms.

        r = MergedExpression(expr, exposed_vars)

        if r in self.merged_expressions:
            r2 = self.merged_expressions[r]
            # then we want to check that the exposed variables are the same, or
            # mark that there are more exposed variables.
            assert r2.exposed_vars == exposed_vars  # TODO: handle != case
            r = r2
        else:
            self.merged_expressions[r] = r
            self.agenda.push(lambda: self._optimize_term(r))  # need to processes this new thing and try the optimizer at it
        return r

    def create_compiled_expression(self, term_ref, exposed_vars: Set[Variable]):
        r = CompiledExpression(term_ref, exposed_vars)

        if r in self.compiled_expressions:
            r = self.compiled_expressions[r]
        else:
            self.compiled_expressions[r] = r

        return r

    def _optimize_term(self, term):
        popt = self.terms_as_optimized.get(term)  # get the current optimized version of the code
        assumpt = self.term_assumption(term)
        if isinstance(term, MergedExpression):
            # then we are going to determine which variables are
            r = term.expression
            exposed = term.exposed_vars
        else:
            name, arity = term  # the name matching the way that we are storing dyna terms
            r = self.terms_as_defined[term]
            exposed = (ret_variable, *variables_named(range(arity)))
        rr, assumptions = run_optimizer(r, exposed)

        assumptions.add(assumpt)

        bc = check_basecases(rr, stack=(term,))
        if bc == 0:
            # then there is no way for this to ever match something, so just report it as empty
            rr = Terminal(0)

        assumption_response = AssumptionResponse(lambda: self._optimize_term(term))
        invalidate = False

        if rr != popt:
            if rr.isEmpty() or popt is not None:
                # then we have "proven" something interesting, so we are going
                # to use the assumption to notify anything that might want to
                # read from this expression.
                invalidate = True
            self.terms_as_optimized[term] = rr

        if invalidate:
            assumptions.remove(assumpt)
            assumptions.add(self.invalidate_term_assumption(term))

        for a in assumptions:
            a.track(assumption_response)


    def _compile_term(self, term, ground_vars :Set[Variable]):
        R = self.lookup_term(term, ('compile', 'memo'))
        if isinstance(R, MergedExpression):
            exposed = term.exposed_vars
        else:
            name, arity = term
            exposed = (ret_variable, *variables_named(range(arity)))

        ce = self.create_compiled_expression(term, exposed)
        incoming_mode = tuple(v in ground_vars for v in ce.variable_order)  # the mode about which variables are ground at the start






# where we will define the builtins etc the base dyna base, for now there will
# just be a single one of these that is global however we should not use the
# global reference whenever possible, as will want to turn this into the
# dynabase references
dyna_system = SystemContext()



class MergedExpression:
    """Used for merginging multiple terms and primitive operations and then storing
    then in the context.  This is created by the optimizers.
    """

    # ??? this seems like the wrong file for this expression.  maybe this file
    # should instead be focused on being the system based definition.  so the
    # pointers to different operations, and then being able to combine a term is
    # just something that requires a new name, and thus we use this

    expression : RBaseType
    exposed_vars : Set[Variable]

    def __init__(self, expression, exposed_vars):
        self.expression = expression
        self.exposed_vars = exposed_vars

    def __eq__(self, other):
        return type(self) is type(other) and self.expression == other.expression

    def __hash__(self):
        return hash(type(self)) ^ hash(self.expression)

    def __repr__(self):
        return f'MergedExpression({self.expression})'


class CompiledExpression:

    def __init__(self, term_ref, exposed_vars :Set[Variable]):
        self.term_ref = term_ref
        self.exposed_vars = exposed_vars
        self.variable_order = tuple(exposed_vars)
        self.compiled_expressions = {}

    def __eq__(self, other):
        return type(self) is type(other) and self.term_ref == other.term_ref and self.exposed_vars == other.exposed_vars

    def __hash__(self):
        return hash(type(self)) ^ hash(self.term_ref) ^ reduce(operator.xor, map(hash, self.exposed_vars), 0)



def check_basecases(R, stack=()):
    # check that this expression can hit some basecase, otherwise this
    # expression must just be terminal zero.  This is needed to get the tree set
    # stuff to work....(as it is sorta the emptyness test)

    # this should be that there is always some branch of a partition that would
    # not encounter something that is on the stack.  If something always
    # encounters whatever is on the stack, then it would always just keep
    # running forever.  So what we are looking for is basically a branch of a
    # partition whcih can avoid calling back to itself.

    # return:
    #   0 then it does not hit a basecase for sure, and we should report an error or just mark this as terminal(0) as it would never be able to terminate
    #   1 unsure as it uses evaluate on all branches so it could go anywhere
    #   2 this is definitly has base cases, but it might be recursive
    #   3 there is no detected recursion

    if isinstance(R, (Evaluate, Evaluate_reflect)):
        return 1  # unsure in this case, we could go /anywhere/
    elif isinstance(R, Partition):
        # partition, so highest score, and only 3 if all branches return 3
        x = 0; z = 3
        for r in R.children:
            y = check_basecases(r, stack=stack)
            if y > x: x = y
            if y < z: z = y
        if z == 3:
            return 3  # no recursion detected on any branch
        return min(x, 2)  # some recursion

    elif isinstance(R, CallTerm):
        if R.term_ref in stack:
            return 0  # hit ourselves in a recursive attempt
        else:
            return check_basecases(R.dyna_system.lookup_term(R.term_ref), stack=stack+(R.term_ref,))
    else:
        # assume intersection, so the lowest score is returned
        x = 3
        for r in R.children:
            y = check_basecases(r, stack=stack)
            if x > y: x = y
            if x == 0: return x
        return x


# class TaskContext:
#     """
#     Represent the context for a given runtime transaction.
#     This is going to be some task that pops of the agenda.
#     The reason for this class is basically we might want to mask some memo table with new updates,
#     or track what operations would need to be pushed to the agenda.
#     The infinite priority agenda should basically be "pushing" something to this local agenda,
#     but then choosing to run it /instantly/ at this point, and then getting the result instead of using whatever the /guessed/ was going to be
#     """
#     def __init__(self, system):
#         self.new_memos = {}
#         self.agenda_additions = []
#         self.system = system



# # this should be a thread local.  The reference to the dyna_system should probably go through this also?
# active_task = TaskContext(dyna_system)
