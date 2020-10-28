class RuntimeController(object):

    def do_simplify_rewrite(self, R, frame):
        return True

    def conjunctive_rewrite_order(self, R, frame, conjuncts):
        return conjuncts

    def disjunctive_rewrite_order(self, R, frame, disjuncts):
        return disjuncts

    def select_loop_variable(self, R, frame, loopable):
        return loopable[0]

    def agenda_pop(self, agenda):
        return agenda.popleft()

    def term_defined(self, name, R):
        return R



# global value which could be called anytime that there are values which
runtime_controller = RuntimeController()
