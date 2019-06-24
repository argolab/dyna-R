
from .interpreter import *
from .terms import BuildStructure

# mode_cache = {
#     term: {
#         in_mode (as a tuple of the arguments) : (out_mode (as a tuple of the arguments), dependants_set)
#     }
# }


class SafetyPlanner:

    def __init__(self, get_rexpr):
        self.mode_cache = {}
        self._agenda = []
        self.get_rexpr = get_rexpr

    def _lookup(self, term, mode):
        cache = self.mode_cache.get(term)

        term_name, arg_names = term  # the name is packed in with the exposed variables
        assert len(arg_names) == len(mode)

        if cache is None:
            # then we have to construct the initial cache the initial state is
            # that all expressions are going to come back as ground.  But then
            # we are going to mark that we have to reprocess the agenda for this
            # expression
            cache = {(False,)*len(mode): ((True,)*len(mode), set())}
            self.mode_cache[term] = cache
            self._push_agenda((term, (False,)*len(mode)))

        if mode in cache:
            return cache[mode]

        # first we check if there is a more restricted mode that matches the
        # requirements for this mode but returns that all of its arguments will
        # be ground
        for kk, v in cache.items():
            if all(m or not k for m,k in zip(mode, kk)) and all(v[0]):
                return v

        # if we are unable to find this, we are going to guess that there is no
        # progress and then push something to the agenda for this mode:
        r = (mode, set())
        cache[mode] = r
        self._push_agenda((term, mode))
        return r

    def _compute(self, term, mode):
        cache = self.mode_cache[term][mode]
        name = (term, mode)
        term_name, exposed_vars = term
        R = self.get_rexpr(term_name)
        out_mode = self._compute_R(R, exposed_vars, mode, name)

        if cache[0] != out_mode:
            self.mode_cache[term][mode] = (out_mode, set())
            for d in cache[1]:
                self._push_agenda(d)  # these need to get reprocessed


    def _compute_R(self, R, exposed_vars, in_mode, name):
        # determine what the true out mode for this expression is by using
        # lookup and collecting the expressions that we are dependant on.

        bound_vars = Frame()  # just set the value of true in the case that something is bound

        for var, im in zip(exposed_vars, in_mode):
            if im:
                var.setValue(frame, True)

        def walker(R):
            if isinstance(R, Partition):
                assert False  # going to have to walk all of the branches and
                              # the merge the variables that are between them to
                              # take the lower bound of what would be bound
            elif isinstance(R, ModedOp):
                # then we can just lookup the modes and determine if we are in
                # one of them.  In which case, then we
                mode = tuple(v.isBound(frame) for v in R.vars)
                if mode in R.det or mode in R.nondet:
                    for v in R.vars:
                        v.setValue(frame, True)
            elif isinstance(R, BuildStructure):
                if R.result.isBound(frame):
                    for v in R.arguments:
                        v.setValue(frame, True)
                elif all(v.isBound(frame) for v in R.arguments):
                    R.result.setValue(frame, True)
            elif isinstance(R, CallTerm):
                # then we need to look this expression up, but that is also
                # going to have to determine which variables are coming back or
                # what the mode is for those expressions.  In this case, we are
                arg_vars = sorted(R.var_map.keys())  # these are the public variables that are exposed from an expression?
                mode = tuple(R.var_map[a].isBound(bound_vars) for a in arg_vars)

                out_mode, tracking = self._lookup((R.term_ref, arg _vars), mode)
                if name:
                    tracking.add(name)  # track that we performed a read on this expression

                # track that this variable is now set
                for av, rm in zip(arg_vars, out_mode):
                    if rm:
                        R.var_map[av].setValue(frame, True)
            else:
                for c in R.children:
                    walker(c)

        while True:
            last_binding = Frame(bound_vars)
            walker(R)
            if last_binding == bound_vars:
                # then this has reached a fixed point for what can be bound, so
                # we stop at this point
                break

        # now this needs to save the result to the cache and if there are
        # differences, then we also will need to push everything to the
        # agenda

        out_mode = tuple(v.isBound(bound_vars) for v in exposed_vars)

        return out_mode

    def _process_agenda(self):
        while self._agenda:
            p = self._agenda.pop()
            self._compute(*p)

    def _push_agenda(self, n):
        if n not in self._agenda:  # use a set?
            self._agenda.append(n)

    def __call__(self, R, exposed_vars, in_mode):
        # do the planning for an R-expr
        while True:
            out_mode = self._compute_R(R, exposed_vars, in_mode, None)
            if not self._agenda:
                break  # meaning that nothing was pushed to work on in the processe
            self._process_agenda()
        return out_mode
