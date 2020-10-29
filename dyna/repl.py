

import re, sys, traceback

#from arsenal import colors
from argparse import ArgumentParser
from copy import deepcopy

from path import Path
import pygments  # list(pygments.styles.get_all_styles())
import pygments.styles
import prompt_toolkit
from prompt_toolkit.formatted_text import ANSI #, HTML
from prompt_toolkit.completion import Completer, Completion
from prompt_toolkit.shortcuts import CompleteStyle
from prompt_toolkit.validation import Validator

from dyna.syntax.pygments_lexer import DynaLexer

# from dyna.answer import Result
# from dyna.aggregators import AGG, UNK
# from dyna.term import Term, same
# from dyna.messages import notify
# from dyna.states import RulePropagateCtx
# from dyna.syntax import Cmd, run_parser, DynaParserException, term
# from dyna.util import colordiff

from dyna.syntax.syntax import DynaParserException, Cmd, term, run_parser
from dyna.syntax.normalizer import add_rules, user_query

from dyna import dyna_system
from dyna import DynaSolverError

from dyna.syntax.util import colors


dotdynadir = Path('~/.dynaR').expand()
if not dotdynadir.exists():
    dotdynadir.mkdir()


hist = dotdynadir / 'dyna.hist'
if not hist.exists():
    with open(hist, 'w') as f:
        f.write('')



class REPL:

    def __init__(self, interp, args=None):
        self.interp = interp
        self.hist = hist
        self.hist_file = hist
        self.lineno = 0
        self.excpt = None
        self.suggested_prompt = None

        self.subscriptions = []
        self.checkpoints = []

        if args is not None:
            for f in args.load:
                self.do_load(f)

    def do_verbose(self, _):
        """
        Toggle solver verbosity.
        """
        self.interp.toggle_verbose()

    # def do_subscribe(self, q: Term):
    #     # TODO: subscription is not just a memo: it creates an obligation, but
    #     # it is immune to invariant 4 the obligation can't be severed by the
    #     # solver or policy - only the user can sever the obligation.
    #     q = str(q)
    #     if not q.strip(): return
    #     if self._query(q) is not None:
    #         d = self.interp
    #         d.memo.memoize(q, d.compute(q))
    #         self.subscriptions.append(q)

    def do_retract_rule(self, idx: int):
        """
        Retract rule from program by rule index.

          > a += 1.
          > b += 1.
          > c += a*b.

        In order to retract a rule we need to know it's index, for that we use
        the command `rules`.

          > rules

          Rules
          =====
            0: a += 1.
            1: b += 1.
            2: c += a * b.

        Now let's remove a rule:

          > retract_rule 0

        This removes rule 0 from the program. Now, let's inspect the changes to
        the solution.

          > sol

          Solution
          ========
          b = 1.

        """

        idx = str(idx)
        try:
            idx = int(idx)
        except ValueError:
            print('Please specify an integer. Type `help retract_rule` to read more.')
        else:
            try:
                r = self.interp.rules.rm(idx)
            except IndexError:
                print('Invalid rule index. Please pick a valid rule index from the listing below.')
                self.do_rules(None)
            else:
                self.interp.init_rule(r, delete=True)

    def do_sol(self, _):
        """
        Show solution.
        """
        # TODO: One possibility for implementing subscriptions to force a each
        # subscribed query; then every update to the dynabase will have to fix
        # these memos.  We can have a callback/hook for when they get updated,
        # which will print the result or change in result.
        for q in self.subscriptions:
            print(q, self._query(q))

    def do_shell(self, _):
        """
        Development tool. Jump into an interactive python shell.
        """

        from dyna import dyna_system
        from IPython import embed; embed()

        # from arsenal import colors
        # from dyna.aggregators import Aggr, UNK, isunk
        # from dyna.answer import Result
        # from dyna.term import Term, RVar, unifies, covers, all_vars, deref
        # from dyna.syntax import term, run_parser

        # d = self.interp


        # from arsenal.debug import ip; ip()

    def _query(self, q):

        try:
            #return self.interp.user_query(q)
            user_query(q)

        except (KeyboardInterrupt, Exception) as e:
            print(colors.red % ''.join(traceback.format_exception(*sys.exc_info())))
            return []

    # def do_obligated_children(self, q: Term):

    #     # TODO: obligations of an invaliation are an overestimate of the
    #     # obligations of other message types.  Do we want to make such options
    #     # available to the user in the REPL?  It's possible that the right API
    #     # is to pass in a dyna terms `$notify(item, $unk)` or
    #     # `$deltaupdate(item, val)`.  Or something like that.  If the outer
    #     # functor isn't message type we can fall back can wrap it in an
    #     # invalidation as a default.
    #     msg = notify(q, UNK, was = UNK)

    #     d = self.interp
    #     with d.memo.no_updates(), d.agenda.no_pushes():
    #         C = [deepcopy(c.item) for c in d.children(msg)]
    #         O = [deepcopy(c) for c in C if d.obl.kids(q)]

    #     print(colors.light.yellow % f'obligated children ({len(O)}/{len(C)})')
    #     for c in sorted(C):
    #         print(f'  {c}:', 'yes' if any(same(c, b) for b in O) else 'no')

    def do_query(self, q):
        """
        Query solution.

        Consider the following example;

          > f(1) := 1.
          > f(2) := 4.

        There a few versions of query:

         - `vquery` shows variable bindings

            > vquery f(X)
            1 where {X=1}
            4 where {X=1}

         - `query` shows variable bindings applied to query

            > query f(X)
            f(1) = 1.
            f(2) = 4.

         - `trace` is an introspection tool for visualizing the derivation of an
           item and its value. Type `help trace` for more information.

        """
        if q is None: return
        results = self._query(q)
        print(results)

    # def do_tickle(self, _):
    #     "Tickle: run each rule's 'initializer' (i.e., tickle it) to see what they emit."
    #     d = self.interp
    #     for r in d.rules:
    #         print()
    #         print(colors.yellow % r.metadata['original'])
    #         for c in RulePropagateCtx(d, r):
    #             print(' ', c)
    #     print()

    def do_pdb(self, _):
        "Enter the debugger at the point of the last exception"
        import ipdb
        ipdb.post_mortem(self.excpt[2])

    def do_debug_on(self, _):
        from dyna import interpreter
        interpreter._PDB_DEBUG = True

    def do_run_agenda(self, _):
        "Run the agenda until convergence"
        dyna_system.run_agenda()

    def do_memoize_null(self, q):
        """Memoize a term using a null default
        Term identified as `name/arity`, eg: `fib/1`
        """
        name, arity = q.split('/')
        arity = int(arity)
        dyna_system.memoize_term((name, arity), kind='null')

    def do_memoize_unk(self, q):
        """Memoize a term using a unknown default
        Term identified as `name/arity`, eg: `fib/1`
        """
        name, arity = q.split('/')
        arity = int(arity)
        dyna_system.memoize_term((name, arity), kind='unk')

    def do_memoize_del(self, q):
        """Delete a memo table
        Term identified as `name/arity`, eg: `fib/1`
        """
        name, arity = q.split('/')
        arity = int(arity)
        dyna_system.memoize_term((name, arity), kind='none')

    def do_optimize(self, q):
        "Run the optimizer on all of the terms in the system"
        dyna_system.optimize_system()


    # def do_memos(self, q: Term):
    #     """
    #     Show the memo.

    #     Optional: Specify a pattern to narrow down the set of memos.
    #     """
    #     print()
    #     self.interp.memo.dump(q)

    # def do_agenda(self, q: Term):
    #     """
    #     Show the agenda.

    #     Optional: Specify a pattern to narrow down the set of message.
    #     """
    #     print()
    #     self.interp.agenda.dump(q)

    # def do_state(self, q: Term):
    #     """
    #     Show the interpreter state (contents of the memo table and the agenda
    #     as well as rules of the program).

    #     Optional: Pass in a term to narrow it down the state as it pertains
    #     to items matching the term.
    #     """
    #     print(self.interp.__str__(q))

    # def do_rules(self, q: Term):
    #     """
    #     List rules in the program

    #     Optional: Pass in a term to narrow it down the set of rules; and
    #     organize the rules into rules for parents and rules for children.
    #     """
    #     print()
    #     print(self.interp.rules.__str__(q))

    # def do_check_invariants(self, q: Term):
    #     """
    #     Run the invariant checker items matching a given pattern.
    #     """
    #     from dyna.invariants import check_invariants
    #     check_invariants(self.interp, q, verbose=True)

    # def do_checkpoint(self, _):   # TODO: could aliases to checkpoints
    #     self.checkpoints.append(str(self.interp))

    # def do_compare(self, _):
    #     if len(self.checkpoints) < 2:
    #         print(colors.light.yellow % 'Need two checkpoints to compare.')
    #         return
    #     a, b = self.checkpoints[-2], self.checkpoints[-1]
    #     print(colordiff(a, b))

    # def do_flush(self, q: Term):
    #     if q is None: q = term('X')
    #     self.interp.memo.flush(q)

    def default(self, line):
        """
        Called on an input line when the command prefix is not recognized.  In
        that case we execute the line as Python code.
        """

        self.add_rules(line)

    # def do_guess(self, q: Term):
    #     """
    #     Make an explict guess about a query's result.  Currently, we only support
    #     making null guessed (plus consistency with the memo table).
    #     """
    #     # TODO: call the policy by default instead of hardcoding null?
    #     # TODO: let the user provide non-null guess.
    #     self.interp.handle_guess(q, Result(default=None))

    def do_run_agenda(self, _):
        self.interp.run_agenda()

    def add_rules(self, line):
        line = str(line)
        line += f'   %% repl line {self.lineno}'

        try:
            #rules_or_cmds = self.interp.parse(line)
            add_rules(line)
        except DynaParserException as e:
            print(type(e).__name__ + ':')
            print(e)
            print('new rule(s) were not added to program.')
            print()
            return ([], {})

        # try:
        #     for r in rules_or_cmds:
        #         if isinstance(r, Cmd):
        #             self.onecmd(r.cmd)
        #             continue
        #         self.interp.add_rules([r])
        # except Exception as e:
        #     print(type(e).__name__, 'error')
        #     print(colors.light.red % (e,))

    def cmdloop(self, _=None):

        # Unfortunately, these thing don't appear to run in a thread that can
        # interrupt the solver.
        bindings = prompt_toolkit.key_binding.KeyBindings()
        @bindings.add('f4')
        def _(event):
            """
            When F4 has been pressed. Insert "hello world" as text.
            """
            #event.app.current_buffer.insert_text('hello world')
            #print('user hit the f4 key')
            self.interp.toggle_verbose(True)

        # TODO: Bottom tool bar is not update in a thread.
        #def get_toolbar():
        #    if self.interp.deadline is not None:
        #        return f'Solver will timeout in {self.interp.deadline - time():g} seconds'

        interp = self.interp

        # remove from completions: pyscript py alias macro EOF eos set exit _relative_load shortcuts quit
        commands = {x[3:] for x in dir(self) if x.startswith('do_')}

        # TODO: create a little mini-framework to completions for different
        # commands; basically call any `complete_{command}` method that exists.

        parse_cmd = self.parse_cmd

        class SlowCompleter(Completer):
            """
            This is a completer that's very slow.
            """
            def __init__(self):
                self.loading = 0

            def get_completions(self, document, complete_event):
                # Keep count of how many completion generators are running.
                self.loading += 1
                word_before_cursor = document.get_word_before_cursor()

                # TODO: add other operators to the set of completions?
                words = {
                    'for',
                }

                from dyna.aggregators import AGGREGATORS
                words.update(AGGREGATORS.keys())
                words.update(commands)
                words.update(name for (name, arity) in dyna_system.terms_as_defined.keys())

                # TODO: add REPL commands to the completions list
                # words.update(AGG)
                # words.update(f for (f,_) in interp.functors)
                # words.update(name for name in commands)

                try:

                    x = parse_cmd(document.text_before_cursor)
                    if x is not None:
                        (cmd, args) = x
                        if cmd == 'load':
                            hacked_text = args
                            yield from prompt_toolkit.completion.filesystem.PathCompleter().get_completions(
                                prompt_toolkit.document.Document(hacked_text), complete_event)
                            return

                    for word in words:
                        if word.startswith(word_before_cursor):
                            #time.sleep(.2)  # Simulate slowness.
                            yield Completion(word, -len(word_before_cursor))

                finally:
                    # We use try/finally because this generator can be closed if the
                    # input text changes before all completions are generated.
                    self.loading -= 1

        # TODO: create a little method annotation framework to make this easier
        # to specify and maintain.  Basically, use argument annotations and make
        # sure that we have a method successfully runs the string->target type
        # conversion.  When we call the method we can use the same stuff except
        # we'll actually call the method with the converted (and validated
        # type).
        validated_as_query = False
        def validate(text):
            nonlocal validated_as_query
            validated_as_query = False
            x = self.parse_cmd(text)
            if x is None: return
            cmd, args = x

            if cmd == 'load':
                return True
            try:
                if cmd in ('query', 'flush', 'memos', 'agenda', 'state'):
                    if args: term(args)
                if cmd == '':
                    if args: run_parser(args)
            except DynaParserException:
                # try to see if this can validate as a query
                # this should probably just work?
                try:
                    run_parser(f'{text} ?')
                    validated_as_query = True
                    return True
                except DynaParserException:
                    pass

                return False
            return True

        session = prompt_toolkit.PromptSession(
            lexer = prompt_toolkit.lexers.PygmentsLexer(DynaLexer),

            completer = SlowCompleter(),
            complete_in_thread = True,
            complete_while_typing = True,
            #bottom_toolbar = bottom_toolbar,
            complete_style = CompleteStyle.MULTI_COLUMN,

            #style = style,
            style = prompt_toolkit.styles.style_from_pygments_cls(pygments.styles.get_style_by_name('paraiso-dark')),   # maybes: paraiso-light, paraiso-dark, arduino, igor, abap, vs
            history = prompt_toolkit.history.FileHistory(self.hist_file),
            enable_history_search = True,
            auto_suggest = prompt_toolkit.auto_suggest.AutoSuggestFromHistory(),
            key_bindings = bindings,
            #bottom_toolbar = get_toolbar, refresh_interval = .25,
            validator = Validator.from_callable(
                validate,
                #error_message='Not a valid e-mail address (Does not contain an @).',
                move_cursor_to_end = True),
            #validate_while_typing = True,
            validate_while_typing = False,
        )

        has_agenda_work = False
        while True:
            if bool(dyna_system.agenda) and not has_agenda_work and not self.suggested_prompt:
                self.suggested_prompt = 'run_agenda '
            has_agenda_work = bool(dyna_system.agenda)
            try:
                suggest = self.suggested_prompt
                self.suggested_prompt = None
                text = session.prompt(
                    ANSI(('(agenda has pending work) ' if has_agenda_work else '')+'\x1b[31m$>\x1b[0m '),
                    **({'default': suggest} if suggest else {})
                )
            except KeyboardInterrupt:
                print('^C')
                continue  # Control-C pressed. Try again.
            except EOFError:
                break  # Control-D pressed.

            try:
                if validated_as_query:
                    user_query(text)
                else:
                    self.runcmd(text)
            except KeyboardInterrupt:
                print('^C')
                self.excpt = sys.exc_info()
                continue  # Control-C pressed. Try again.
            except Exception as err:
                self.excpt = sys.exc_info()
                print(colors.red % ''.join(traceback.format_exception(*self.excpt)))
                if isinstance(err, DynaSolverError):
                    print(colors.yellow % err)
                    if hasattr(err, 'suggest'):
                        self.suggested_prompt = err.suggest

    def parse_cmd(self, x):
        commands = {x[3:] for x in dir(self) if x.startswith('do_')}
        try:
            [(cmd, args)] = re.findall(f'^((?:{"|".join(commands)}\s+)|)(.*)$', x)
            return [cmd.strip(), args.strip()]
        except ValueError:
            return ['', x]

    def runcmd(self, text):
        if not text.strip(): return

        #from dyna.debug.tracer import Tracer   # reset the tracer's depth to avoid unwanted state
        #Tracer.call_depth *= 0
        self.lineno += 1

        [cmd, args] = self.parse_cmd(text)
        cmd = getattr(self, 'do_' + cmd, self.default)

        # a = cmd.__annotations__
        # if len(a) == 1:
        #     [(_,a)] = a.items()
        #     try:
        #         if a is Term:
        #             args = term(args) if args.strip() else None
        #     except DynaParserException as e:
        #         print('Parser error:', e)
        #         return

        return cmd(args)

    def do_help(self, line):
        # create help routines based on doc string.
        if line:
            method = getattr(self, f'do_{line.strip()}', None)
            if method and hasattr(method, '__doc__'):
                print(method.__doc__)
            else:
                print(f'{line} not found')
        else:
            for x in dir(REPL):
                v = getattr(self, x)
                if x.startswith('do_') and getattr(v, '__doc__', None):
                    v = getattr(self, x)
                    print(x[3:], v.__doc__)

    def do_load(self, line: Path):
        "Load rules from a dyna source file."
        line = str(line)
        with open(line.strip()) as f:
            add_rules(f.read())
            #self.add_src(f.read())

    def do_quit(self, *a):
        sys.exit(0)
    do_exit = do_quit


def main():
    # increase the stack size so that this can run long backchaining sequences
    import resource
    resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    sys.setrecursionlimit(20_000)

    repl = REPL(dyna_system)
    dyna_system.run_agenda()  # stuff pushed from the initial load
    repl.cmdloop()

if __name__ == '__main__':
    main()
