from collections import defaultdict

from .interpreter import *
from .builtins import *
from .terms import CallTerm, BuildStructure
from .guards import Assumption, get_all_assumptions
from .aggregators import AGGREGATORS
from .context import MergedExpression

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

GRADIENT_FUNC = '$__gradient_func_{name}/{arity}'
GRADIENT_ACCUMULATOR = '$__gradient_accumulator_{name}/{arity}'

#GRADIEN_RES = VariableId('$__gradient_source')

def get_body(R):
    if isinstance(R, (Partition, ModedOp, FinalState, Aggregator, BuildStructure)):
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


        # these are things which sum into the gradient based of places where it is used
        gradient_sums = defaultdict(list)

        # these are gradients wrt the arguments to a function
        gradient_func = {}

        builtin_only = set()

        def call_gfunc(name, arity):
            return self.dyna_system.call_term(GRADIENT_FUNC.format(name=name,arity=arity), arity)

        for name in predicate_set:
            func = self.dyna_system.lookup_term(name, ignore=('memo', 'compile', 'not_found'))
            func_call = self.dyna_system.call_term(*name)
            # this needs to identify all of the branches on this expression if
            # there is no aggregator, or this is just a builtin, then I suppose
            # that we should ignore it?

            for assumpt in get_all_assumptions(func):
                assumpt.track(new_assumption)

            body = get_body(func)

            if isinstance(name, tuple) and len(name) == 2:
                exposed_variables = variables_named(*range(name[1]))
                source_gradient_name = GRADIENT_ACCUMULATOR.format(name=name[0], arity=name[1])
            elif isinstance(name, MergedExpression):
                exposed_variables = name.exposed_vars
                source_gradient_name = None
                assert False  # TODO: need to identify some name for this to be the source gradient for??? (hash the name or something I guess, or have some way of uniquely identifying what expression is used)
            else:
                assert False  # other cases that need to be handled?


            if body is None:
                assert False  # this should hopefully not happen
            elif isinstance(body, ModedOp):
                which_op = None
                arity = None
                for mops, opn in modedop_gradients.items():
                    if mops.possibly_equal(body):
                        which_op = opn
                        arity = len(mops.vars)
                        break
                assert which_op  # otherwise there is something else we need to add for what the gradient is defined as

                # link the definition of the gradient function that is being defined to what is already defined
                gradient_func[name] = self.dyna_system.call_term(f'$gradient_{which_op}', arity)
                builtin_only.add(name)
            elif isinstance(body, FinalState):
                gradient_func[name] = body
            elif isinstance(body, BuildStructure):
                # if there is not something that can be unpacked with the
                # gradient, then this I think that this is right?  Need to
                # unpack the tuples and their gradients and repack them back as
                # two different operations?

                vals, grads = [],[]
                Rs = []
                for arg in body.arguments:
                    v = VariableId()
                    g = VariableId()
                    Rs.append(BuildStructure('$', arg, (v, g)))

                v = VariableId()
                g = VariableId()
                Rs.append(BuildStructure(body.name, v, vals))
                Rs.append(BuildStructure(body.name, g, grads))
                Rs.append(BuildStructure(body.result, '$', (v,g)))

                gradient_func[name] = intersect(*Rs)

            elif isinstance(body, Aggregator):
                # determine the name of the aggregator?
                is_selective = body.aggregator.selective

                # for all of the branches, this needs to identify which value is
                # a source, and add those to the accumulation of those values.
                # For anything that is just a representation

                # does this need to handle the merged expressions?  This should
                # just be able to operate on the named expressions directly
                # gargs = [VariableId(f'$__gradient_arg_{i}') for i in range(name[1])]
                source_gradient = self.dyna_system.call_term(source_gradient_name, name[1])#(*gargs, ret=GRADIEN_RES)


                # if is_selective:
                #     # then use the value from the branch that is selected to
                #     # determine which element should be used.  Otherwise, we are
                #     # going to have to

                #     # want to identify which expression is in the head of the variables, and what the result variables will be
                #     res_var = VariableId()
                #     vmap = {x:x for x in variables_named(*range(name[1]))}
                #     vmap[ret_variable] = res_var
                #     # conceptually, this could expose other variables that are
                #     # in the expression though that might not work in the case
                #     # of memoization?
                #     # this also needs to manage which are the source variables and what are the destination variables as those are no longer the same expression
                #     selecting = intersect(self.dyna_system.call_term('==', 2)(res_var, body.body_res),
                #                           CallTerm(vmap, self.dyna_system, name))
                # else:
                #     # this just selects everything, so this will just get this removed in the end
                #     selecting = Terminal(1)

                children_branches = body.body._children  # this is from the partion branches
                assert isinstance(children_branches, PrefixTrie)
                uvars = body.body._unioned_vars

                def transform_body(key, value):
                    # ensure that the values for every key are embedded in the expression (specalized)
                    # otherwise, this might incorrectly forget something?

                    gargv_vals = tuple(constant(k) if k is not None else VariableId() for k in key)
                    gres = VariableId()
                    fres = VariableId()

                    sgf = source_gradient(*gargv_vals, ret=gres)
                    scf = func_call(*gargv_vals, ret=fres)

                    gval = VariableId()

                    cv = intersect(BuildStructure('$', gval, (fres, gres)), scf, sgf)

                    # if is_selective:
                    #     cv = intersect(cv,

                    rename_map = {VariableId(i):v for i,v in enumerate(gargv_vals)}
                    rename_map[body.body_res] = gval

                    vr = value.rename_vars_unique(rename_map.get)

                    called_funcs = []

                    def rename_func(R):
                        # rename the functions such that it calls the gradient equivalent functions
                        if isinstance(R, CallTerm):
                            called_funcs.append(R)
                            # this just needs to change the term ref on the expression, the variable names should stay the same
                            oname, arity = R.term_ref
                            oname = GRADIENT_FUNC.format(name=oname, arity=arity)
                            nr = CallTerm(R.var_map, R.dyna_system, (oname, arity))
                            import ipdb; ipdb.set_trace()
                            return nr
                        return R.rewrite(rename_func)

                    vr = vr.rewrite(rename_func)

                    for cf in called_funcs:
                        pass

                    # rvals.append(intersect(cv,
                    #                        *(Unify(v, u) for v, u in zip(key, uvars) if v is not None), value))
                    return intersect(cv, vr)


                func_grad = []
                for key, values in children_branches.items():
                    for value in values:
                        func_grad.append(transform_body(key, value))

                # func_grad represents the different branches of the gradient
                # that should be identified with

                # for branch in children_branches.values():
                #     # there are values

                print(children_branches)

                import ipdb; ipdb.set_trace()

                pass
            elif isinstance(body, Partition):
                # thisi s odd?  Not sure that we are actually going to get this
                # back from an expression which isn't folded or something.  The
                # semiring would just be the multiplicies which are the results,
                # though we are not computing the gradient wrt those values
                assert False


        for b in builtin_only:
            # things which are builtin do not need to compute the accumulated gradient, as we are not going to use these values
            # limits the number of things that would change
            gradient_sums.pop(b, None)

        import ipdb; ipdb.set_trace()

        for name, body in gradient_func.items():
            # if there is another expression here that is already equal to this expression, then we should avoid redefining the
            # expression, otherwise we could end in in a cycle
            self.dyna_system.define_term(name[0], name[1], body)

        for name, body in gradient_sums.items():
            # this is going to construct a sum aggregator which accumulates from all of the different branches
            # then it will want to

            pass

        # this needs to define the new functions.  If the functions are the
        # same, then they should probably not get redefined (I suppose).
        #
        # the gradient sum will be everything that feeds into a given function.

def define_gradient_operations(dyna_system):

    gradient = GradientCircuit(dyna_system)

    dyna_system.add_rules("""
    $loss += 0.  % this is the loss that the entire program is differenated against

    %$gradient_inputs(&'+'(A, B), InGrad) = x(InGrad, InGrad).
    %$gradient_self(&'+'(A, B), Out,

    % the system will start by looking here though, rather than at the defintion of loss, do not override
    '$__true_loss' = '$loss'().

    % base cases for the gradient to get the computation started
    '$__gradient_accumulator_$__true_loss/0'() += 1.
    '$__gradient_func_$__true_loss/0'($($loss, 1)).


    $gradient(X) =
      $reflect(X, Name, Arity, Args), GName = "$__gradient_accumulator_"+Name+"/"+cast_str(Arity),
      $reflect(COp, GName, Arity, Args), $call(COp).

    % gradients for builtin moded operations.  These have to support all of the
    % same modes, so the gradient may come in from /any/ argument, and need to
    % go out on any other argument

    $gradient_add($(A+B, G),      $(A, G), $(B, G)).
    $gradient_mul($(A*B, G),      $(A, G/B), $(B, G/A)).
    $gradient_abs($(abs(A), G),   $(A, sign(A)*G)).
    $gradient_sin($(sin(X), G),   $(X, cos(G))).
    $gradient_cos($(cos(X), G),   $(X, sin(G))).
    $gradient_tan($(tan(X), G),   $(X, 1/(cos(G)^2))).

    $gradient_exp($(exp(X), G),   $(X,exp(G))).
    $gradient_pow($(X^Y, G),      $(X, Y*(G^(Y-1))), $(Y,  [todo()])).  % TODO: gradient for exponent
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
