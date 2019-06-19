from .interpreter import *
from .terms import *


convert_R_to_term = Visitor(track_source=False)

def convert_var_to_term(var: Variable):
    if isinstance(var, ConstantVariable):
        return Term('$constant', (var.getValue(None),))
    elif isinstance(var, VariableId):
        return Term('$variable', (str(var),))

@convert_R_to_term.default
def convert_R_to_term_default(R):
    raise NotImplementedError()


@convert_R_to_term.define(Intersect)
def convert_R_to_term_intersect(R):
    return Term('$intersect', (Term.fromlist([convert_R_to_term(c) for c in R.children]),))

@convert_R_to_term.define(Partition)
def convert_R_to_term_partition(R):
    pass



@convert_R_to_term.define(Unify)
def convert_R_to_term_unify(R):
    return Term('$unify', (convert_var_to_term(R.v1), convert_var_to_term(R.v2),))


@convert_R_to_term.define(Aggregator)
def convert_R_to_term_agg(R):
    return Term('$aggregator', (convert_var_to_term(R.result),
                                Term.fromlist([convert_var_to_term(v) for v in R.head_vars]),
                                convert_var_to_term(R.body_res),
                                # R.aggregator......
                                "+=",  # there needs to be some global map table of these names so we can lookup the operation
                                convert_R_to_term(R.body)))

@convert_R_to_term.define(ModedOp)
def convert_R_to_term_moded(R):
    # this needs to lookup the name of the operation?  we are going to have to handle the
    assert False
    return Term('$call', ())


@convert_R_to_term.define(CallTerm)
def convert_R_to_term_callTerm(R):
    assert False
    # if the term ref is not something that we can encode, like it is an
    # internal name, or something that we concocted from being the result of
    # merging multiple expressions together, I suppoose that we should just
    # error out in that case we should represent the returned value for some
    # expression also.
    name, arity = R.term_ref

    # this should try and match the output from the parser as much as possible.  then it might make the interface a bit better?
    return Term(name, (...))


@convert_R_to_term.define(BuildStructure)
def convert_R_to_term_buildstructure(R):
    return Term('&', Term(R.name, ...))
