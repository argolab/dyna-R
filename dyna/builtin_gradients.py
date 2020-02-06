from collections import defaultdict

from .interpreter import *
from .builtins import *
from .terms import CallTerm
from .guards import Assumption, get_all_assumptions
from .aggregators import AGGREGATORS

# automatically differenate code on demand.  This should generate synthetic
# methods which identify which computations are performed.

# if there are more than one gradient parameters, then this might become icky?
# Or would this need to match against.  This would mean that having something
# like:
#   $parameteres_next(X) := $parameteres(X) - alpha * $gradient($loss, $paramteres(X)).


# the gradients need to be in terms of the R-exprs not the names of the
# expressions.  These expressions can be rewritten, which means that the
# operations might not be someting that we can match easily.  In the case that
# there is a call operation, then we are going to want to lookup the body of the
# call.  There might be operations which are the result of an aggregation or
# something, though those are cases are going to have to handle if there are

modedop_gradients = {
    add: 'add',
    mul: 'mul',
    abs_v: 'abs',
    int_div: 'int_div',
    sin_r: 'sin',
    cos_r: 'cos',
    tan_r: 'tan',
    sinh_r: 'sinh',
    cosh_r: 'cosh',
    tanh_r: 'tanh',
    exp_r: 'exp',
    pow_v: 'pow',
    mod_v: 'mod'
}


def get_body(R):
    if isinstance(R, (Partition, ModedOp, FinalState, Aggregator)):
        return R
    elif len(R.children) == 1:
        return get_body(R.children[0])

class GradientCircuit(object):

    def __init__(self, dyna_system):
        self.dyna_system = dyna_system
        self.assumption = None

    def generate_gradient(self):
        # there was some assumption that might have been invalidated, or new
        # code that was added, we are giong to recompute all of the operation

        new_assumption = Assumption('gradient loss')

        to_process_predicates = [('$__true_loss', 0)]
        predicate_set = set(to_process_predicates)

        while to_process_predicates:
            func = self.dyna_system.lookup_term(to_process_predicates.pop(), ignore=('memo', 'compile', 'not_found'))
            for child in func.all_children():
                if isinstance(child, CallTerm) and child.term_ref not in predicate_set:
                    predicate_set.add(child.term_ref)
                    to_process_predicates.append(child.term_ref)

        # now we should have a complete list of all predicates which are used in the computation of the gradient


        gradient_sums = defaultdict(list)

        for name in predicate_set:
            func = self.dyna_system.lookup_term(name, ignore=('memo', 'compile', 'not_found'))
            # this needs to identify all of the branches on this expression if
            # there is no aggregator, or this is just a builtin, then I suppose
            # that we should ignore it?

            for assumpt in get_all_assumptions(func):
                assumpt.track(new_assumption)

            body = get_body(func)

            import ipdb; ipdb.set_trace()
            if body is None:
                assert False  # this should hopefully not happen
            elif isinstance(body, (ModedOp, FinalState)):
                # this is a base case, which is not something that we are going
                continue
            elif isinstance(body, Aggregator):
                pass
            elif isinstance(body, Partition):
                # thisi s odd?  Not sure that we are actually going to get this
                # back from an expression which isn't folded or something.  The
                # semiring would just be the multiplicies which are the results,
                # though we are not computing the gradient wrt those values
                assert False




        pass

def define_gradient_operations(dyna_system):

    gradient = GradientCircuit(dyna_system)

    dyna_system.add_rules("""
    $loss += 0.  % this is the loss that the entire program is differenated against

    $gradient_inputs(&'+'(A, B), InGrad) = &x(InGrad, InGrad).
    %$gradient_self(&'+'(A, B), Out,

    % the system will start by looking here though, rather than at the defintion of loss, do not override
    '$__true_loss' = '$loss'().
    """)

    dyna_system.agenda.push(gradient.generate_gradient)

    #import ipdb; ipdb.set_trace()

    # dyna_system.watch_term_changes(('$loss', 0),)

    # dyna_system.add_rules("""
    # % gradient of an expression without its arguments that it might depend on
    # $gradient(Of) =
    #    $reflect(Of, Name, Arity, Args), GName = "$__gradient_"+Name+"/"+cast_str(Arity),
    #    $reflect(COp, GName, Arity, Args), $call(COp).

    # '$__gradient_+/2'(A, B) = &x(1, 1).
    # '$__gradient_*/2'(A, B) = &x(B, A).

    # $gradient(WRT, OfPredicate) = $reflect(OfPredicate, Name, Arity, _), GName = "$__gradient_"+Name+"/"+cast_str(Arity)
    # """)
