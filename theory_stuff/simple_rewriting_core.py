from dyna.terms_impl import Term


term_rewrite_meta = {
    'builtin_plus':  ... ,
    'builtin_times': ... ,

}



class RewriteOperationBase:

    pass


class RewriteOperationIntersection(RewriteOperationBase):
    name = '*'

    def rewrite(self, term, context):
        for i in range(term.arity):
            # this should do the rewrite of the given expressions


@define_rewrite('+', None)
def rewrite_plus(term):
    # if there is something that
    multiplicity = 0
    multiplicity_cnt = 0
    for i in range(term.arity):
        v = term.get_argument(i)
        if isinstance(v, int):
            multiplicity += v
            multiplicity_cnt += 1
    if multiplicity_cnt > 1:
        if multiplicity_cnt == term.arity:
            # everything reduced, so just return a multiplicity
            return multiplicity
        else:
            sub_terms = [multiplicity]
            for i in range(term.arity):
                v = term.get_argument(i)
                if not isinstance(v, int):
                    sub_terms.append(v)
            return term.create_term('+', sub_terms)
    # there are no rewrites for thi sitem, so we are going to return None to indicate that nothing is changing
    return None


@visit_children('+', None)
def visit_children_plus(term):
    yield from range(term.arity)  # this could just be the slots where there are children present



@define_rewrite('builtin_add', 3)
def builtin_add(term):
    # there should be the different modes here for the builtin operation
    a,b,c = term.get_argument(0), term.get_argument(1), term.get_argument(2)
    num = (int, float)
    if isinstance(a, num) and isinstance(b, num) and isinstance(c, num):
        # then all are bound, so we should just check if the assignment works
        return int(a + b == c)
    elif isinstance(a, num) and isinstance(b, num):
        # C should be of type VariableUnbound, in which case, this will want to identify
        # so this will then want to rewrite
        return term.create_term('=', [c, a+b])
    ...
    # this would match the other nodes
    # so the values which are stored in the language, this is not going to find that there are some values
    # this would perform that this is

    # this is looking like the values which are stored in the structure are just
    # values which correspond with the constant values

    # so when there is something like builtin_plus(1,2,Variable(FOO))  this would get rewritten as
    # (Variable(FOO)=3)
    # from that point, it would be able to take the value which is the result of this expression and store it into the frame
    #
