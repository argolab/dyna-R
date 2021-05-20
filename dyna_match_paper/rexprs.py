# This implementation of R-exprs is designed to be _as simple as possible_
# and match the peseudo code in the appendix _as closely as possible_
#
# Additionally, this version of the code contains lots of logging operations to
# trace what is happening.  This is used to generate the traces which are
# contained in the appendix of the paper.


import sys
import os
import atexit
import uuid
import math

from collections import defaultdict
from functools import cache
from textwrap import indent

from contextlib import contextmanager


####################################################################################################
# utility functions

@cache
def parse_sexp(s):
    # super basic s-expression parser
    sexpr = s.replace('(', ' ( ').replace(')', ' ) ').split()  # lex the expression
    stack = [[]]
    for symbol in sexpr:
        if symbol == '(':
            stack.append([])  # push stack
        elif symbol == ')':
            val = tuple(stack.pop())
            stack[-1].append(val)
        else:
            stack[-1].append(symbol)
    if len(stack) != 1:
        raise RuntimeError(f'S Expression is ill-formed: {s}')
    return tuple(stack[0])


####################################################################################################
# for helping with tracking the rewrites (to generate "figures" in the paper)

logging_rewrites = True
active_rewrite = []
logging_rewrite_output = None
color_output_latex = False
color_terminal = False
generating_latex_output = False
logging_rewrites_temp_disabled = False
limit_printed_amount = True  # try and replace expressions which are duplicated with ... so that they do not distract from the core of what is happening

event_log = []
latex_log = []


@contextmanager
def named_rewrite(name):
    """
    This can be wrapped around the matching expression such that we can log which name should be applied to an expression
    These can then be used to generate output which appears
    """
    global active_rewrite
    active_rewrite.append(name)
    try:
        yield None
    finally:
        assert active_rewrite[-1] == name
        active_rewrite.pop()

@contextmanager
def no_logging_rewrites():
    global logging_rewrites_temp_disabled
    prev_logging_rewrites = logging_rewrites_temp_disabled
    logging_rewrites_temp_disabled = True
    try:
        yield None
    finally:
        logging_rewrites_temp_disabled = prev_logging_rewrites

def track_rexpr_constructed(func):
    if not logging_rewrites:
        return func
    def nf(*args, **kwargs):
        r = func(*args, **kwargs)
        if isinstance(r, Term):
            # maybe this should only include some of the arguments
            # ignore the kwargs if there are any passes a those are just other additional information
            r._debug_constructed_from.append(args)
        return r
    nf.__name__ = func.__name__
    nf.__doc__ = func.__doc__
    return nf

def track_constructed(rexpr, rewrite, text_description, source):
    if not logging_rewrites:
        return rexpr
    if not isinstance(rewrite, str):
        # more than one rewrite being used at the same time is passed as a list or tuple
        rexpr._debug_active_rewrite.extend(rewrite)
    else:
        rexpr._debug_active_rewrite.append(rewrite)
    log_event('applied_rewrite', rewrite, source, text_description, rexpr)
    return rexpr

def log_event(event_kind, *args):
    assert event_kind in ('simplify_fast', 'simplify_full', 'memo_indeterminate', 'memo_computed', 'memo_looked_up', 'applied_rewrite', 'original_rexpr', 'rewritten_result', 'memo_compute_start', 'memo_compute_end')
    if logging_rewrites and not logging_rewrites_temp_disabled:
        # we will want to somehow register that this is doing the different operations
        event_log.append((event_kind, args))
        print(event_kind, args)

def latex_verbatim_block(text):
    # make a verbatim block using \verb expressions with \verb" as the escape sequence
    # if an expression is like we still want for there to be some escape sequence, so that we can encode that
    # the escape sequnce will be [[[ raw latex code ]]]
    # so we

    color_options = ['green', 'blue', 'orange', 'purple', 'brown', 'cyan', 'magenta']
    color_idx = 0
    res = []
    paren_order = []
    for c in text:
        if c in '()': paren_order.append(c)
    paren_idx = 0
    for c in text:
        if c == '(':
            if paren_order[paren_idx+1] != ')':
                res.append(r'[[[{\color{'+color_options[color_idx % len(color_options)] +r'} \Verb|(|}]]]')
                color_idx += 1
            else:
                res.append(c)  # just make this black
            paren_idx += 1
        elif c == ')':
            if paren_order[paren_idx-1] != '(':
                color_idx -= 1
                res.append(r'[[[{\color{'+color_options[color_idx % len(color_options)] +r'} \Verb|)|}]]]')
            else:
                res.append(c)  # just make this black
            paren_idx += 1
        else:
            res.append(c)
    assert color_idx == 0  # make sure everything matched
    text = ''.join(res)

    text = text.strip().replace('"', r'"\Verb|"|\Verb"').replace('\n', '" \\\\\n\\Verb"').replace('[[[', '"').replace(']]]', '\\Verb"')
    text = '\\Verb"' + text + '"'
    text = text.replace(r'\Verb""', '')  # delete useless verb blocks
    return text

def focus_term_print(to_print, alt):
    # remove parts of the R-expr which are the same in the alt expression.  This
    # should make the printed R-exprs smaller and easier to read as a result
    s = {}
    def a(x):
        if x._debug_term_size > 100:
            s[x] = Term('...', ())
    walk_rexpr(alt, a)
    return replace_term(to_print, s)

def reset_rexprs_to_print(rexpr):
    def a(x):
        x._debug_printer_controller = 'NO'
    walk_rexpr(rexpr, a)
    return rexpr

def reset_print_everything(rexpr):
    def a(x):
        x._debug_printer_controller = 'YES'
    walk_rexpr(rexpr, a)
    return rexpr

def identify_interesting_part_of_rewrite(from_source, destination):
    global object_identifier_counter
    obj_source = object()
    obj_dest = object()
    sources = set()
    dests = set()
    both_count = 0
    both = []
    uniques = []

    def similar_exprs(a,b):
        if a == b:
            return True
        return False
        # if a.name != b.name:
        #     return False
        # if a.name == 'proj':
        #     return a.arguments[0] == b.arguments[0]
        # if a.name == 'aggregate':
        #     return a.arguments[0:3] == b.arguments[0:3]
        # if a.name in ('*', '+'):
        #     return False  # meaning that there is a difference between their nested arguments
        return False

    def ps(k):
        def f(x, parent=None):
            if x.name == 'proj':
                parent = ('proj', x.arguments[0])
            elif x.name == 'aggregator':
                parent = ('aggregator', x.arguments[0:3])
            for a in x.arguments:
                if isinstance(a, Term):
                    setattr(a, k, parent)
                    f(a, parent)
        return f
    ps('_debug_parent_term_source')(from_source, 'root')
    ps('_debug_parent_term_dest')(destination, 'root')

    def c(x):
        x._debug_identified_as_same = None

    def s(x):
        x._debug_identified_as_same = obj_source
        sources.add(x)
    def d(x):
        nonlocal both_count
        if getattr(x, '_debug_identified_as_same', None) == 'both':
            return  # this is already marked as in both
        elif getattr(x, '_debug_identified_as_same', None) is obj_source:
            x._debug_identified_as_same = 'both'
            both_count += 1
            both.append(x)
        else:
            #assert x not in sources or x._debug_term_size < 15
            dests.add(x)
            x._debug_identified_as_same = obj_dest

    walk_rexpr(from_source, c)
    walk_rexpr(destination, c)
    walk_rexpr(from_source, s)
    walk_rexpr(destination, d)  # now we are going to walk all of the fi

    def u(x):
        if x._debug_identified_as_same != 'both' or not similar_exprs(getattr(x, '_debug_parent_term_source', Term('*', ())), getattr(x, '_debug_parent_term_dest', Term('*', ()))):
            uniques.append(x)
            x._debug_printer_controller = 'YES'
    walk_rexpr(from_source, u)
    walk_rexpr(destination, u)

    # this prevents it from printing something like ...*...*..., but instead will just print ... for the omitted parts
    def u(x):
        if x.name in ('*', '+'):
            x._debug_printer_controller = 'yes'
            return
            all_noprint = True
            for a in x.arguments:
                if a._debug_printer_controller != 'NO':
                    all_noprint = False
            if all_noprint:
                # all of the children are the same, so don't print this element specifically
                x._debug_printer_controller = 'NO'
                return
    walk_rexpr(from_source, u)
    walk_rexpr(destination, u)




    # something which is not contained in both should be printed, though this will want
    # to select the smallest objects which



def color(c, r):
    if color_terminal:
        color_map = {
            'vargreen': '\033[92m',
            'aggblue': '\033[94m'
        }
        return color_map[c] + r + '\033[0m'
    elif color_output_latex:
        return '[[[{\\color{'+c+'}]]]' + r + '[[[}]]]'  # this will generate something like {\color{xxx}\Verb"...."}
    else:
        return r

def generate_latex():
    global color_output_latex, generating_latex_output
    color_output_latex = True
    generating_latex_output = True
    #simplify_count = 0
    simplify_fast_count = 0
    simplify_full_count = 0
    try:
        ret = []
        source_order = []
        rewrites_performed = defaultdict(list)
        for kind, args in event_log:
            if kind == 'original_rexpr':
                rexpr, = args
                ret.append(r'\textbf{Original query represented as a \rexpr:} \\')
                ret.append(latex_verbatim_block(rexpr.stylized_rexpr()))
                ret.append(r'\\')
                ret.append(r'{\centering \rule{4cm}{0.4pt}} \\')

                if rewrites.user_defined_rewrites_used:
                    ret.append(r'\textbf{Additional user defined rewrites used for rewritting:} \\ \nopagebreak')
                    ret.append(r'\begin{longtable}{ccc}')
                    for (name, arity) in rewrites.user_defined_rewrites_used:
                        t = Term(name, (Variable(i) for i in range(arity)))
                        ret.append(r'\begin{minipage}{.4\textwidth}')
                        ret.append(latex_verbatim_block(t.stylized_rexpr()))
                        ret.append(r'\end{minipage}')
                        ret.append(r'& $\to$ &')
                        ret.append(r'\begin{minipage}{.4\textwidth}')
                        ret.append(latex_verbatim_block(rewrites.user_defined_rewrites[(name, arity)].stylized_rexpr()))
                        ret.append(r'\end{minipage} \\[2pt]')
                    ret.append(r'\end{longtable}')

            elif kind == 'simplify_fast' or kind == 'simplify_full':
                old, rexpr = args

                if rewrites_performed:
                    ret.append(r'\textbf{Rewrites applied:} \\ \nopagebreak')
                    ret.append(r'\begin{longtable}{ccc}')
                    rewrites_done = {}
                    generated_lines = 0
                    last_textual_description = None
                    for s in source_order:
                        # we will want to generate some kind of two column table with arrows between them
                        if isinstance(s, tuple):
                            # meaning that this is some result that we are going to apply to the
                            rewrites_done[s[0]] = s[1]
                            continue
                        dests = rewrites_performed[s]
                        cnt = 0
                        # if generated_lines != 0:
                        #     ret.append(r'[2pt] \hline \\[2pt]')
                        generated_lines += 1
                        ns = replace_term(s, rewrites_done)  # if this is at some intermediate step, then something might have changed
                        reset_rexprs_to_print(ns)
                        for _,_, dest_rexpr in dests:
                            reset_rexprs_to_print(dest_rexpr)
                        for _,_, dest_rexpr in dests:
                            identify_interesting_part_of_rewrite(ns, dest_rexpr)
                        for which_rewrite, textual_description, dest_rexpr in dests:
                            if textual_description != last_textual_description:
                                if generated_lines > 1:
                                    ret.append(r'[2pt] \hline \\[2pt]')
                                ret.append(r'\multicolumn{3}{l}{\parbox{.95\textwidth}{' + textual_description + r'}} \\[8pt] \nopagebreak')
                                last_textual_description = textual_description
                            else:
                                if generated_lines > 1:
                                    ret.append(r' \\[2pt]')
                            #ret.append(textual_description + r' & & \\')
                            if cnt == 0:
                                # only print out the source rewrite once, this will want to combine the colums if there are multiple expressions
                                # if len(dests) != 1:
                                #     ret.append(r'\multirow{'+str(len(dests))+r'}{*}{')
                                ret.append(r'\begin{minipage}{.4\textwidth}')
                                ret.append(latex_verbatim_block(ns.stylized_rexpr()))
                                ret.append(r'\end{minipage}')
                                # if len(dests) != 1:
                                #     ret.append(r'}')
                                cnt += 1

                            if isinstance(which_rewrite, str):
                                which_rewrite_str = r'\ref{' + which_rewrite + r'}'
                            else:
                                # there might be multiple rewrites which applied here
                                # so these were registered as some macro step. so report all of them combined
                                which_rewrite_str = r'\ref{' + r'}, \ref{'.join(which_rewrite) + r'}'

                            ret.append(r' & ${\todocolor{red}\xrightarrow{{\todocolor{blue!50}\footnotesize' + which_rewrite_str + r'}}}\xspace$ & \begin{minipage}{.4\textwidth}')
                            #ret.append(r'} & junk & \mbox{')
                            #dest_limit = focus_term_print(dest_rexpr, ns)
                            ret.append(latex_verbatim_block(dest_rexpr.stylized_rexpr()))
                            ret.append(r' \end{minipage} \\')

                    ret.append(r'\end{longtable}')

                if kind == 'simplify_fast':
                    simplify_fast_count += 1
                else:
                    assert kind == 'simplify_full'
                    simplify_full_count += 1
                #simplify_count += 1
                ret.append(r'\textbf{\rexpr after applying \hyperref[function:simplify_once_fast]{\textsc{\SimplifyOnceFast}} ' f'{simplify_fast_count} ' + ('time' if simplify_fast_count <= 1 else 'times') + ((r' and \hyperref[function:simplify_once_fully_partition]{\textsc{\SimplifyOnceFullyPartition}} ' f'{simplify_full_count} ' + ('time' if simplify_full_count <= 1 else 'times')) if simplify_full_count > 0 else '')+ r':} \\')

                ret.append(latex_verbatim_block(reset_print_everything(rexpr).stylized_rexpr()))
                ret.append(r'\\')
                ret.append(r'{\centering \rule{4cm}{0.4pt}} \\')
                rewrites_performed = defaultdict(list)
                source_order = []

            elif kind == 'applied_rewrite':
                which_rewrite, source_rexpr, textual_description, resulting_rexpr = args
                # if the source \rexpr has been
                if source_rexpr not in rewrites_performed:
                    source_order.append(source_rexpr)
                rewrites_performed[source_rexpr].append((which_rewrite, textual_description, resulting_rexpr))

            elif kind == 'rewritten_result':
                orig_rexpr, new_rexpr = args
                source_order.append((orig_rexpr, new_rexpr))

            elif kind == 'memo_indeterminate':
                ret.append(r'\textbf{Memo access operation deferred:}')
                (name, arity), arg_values = args
                r = name + '(' + ', '.join(
                    [str(a) if a is not None else r'[[[\textit{free}]]]' for a in arg_values]) + ')'
                ret.append(latex_verbatim_block(r))
                ret.append(r'\\')

            elif kind in ('memo_computed', 'memo_looked_up'):
                # then we should print something here for this
                (name, arity), arg_values, returned_rexpr = args
                ret.append(r'\textbf{Memo ' + ('Computed' if kind == 'memo_computed' else 'Looked Up') + r'}')
                r = name + '(' + ', '.join(
                    [str(a) if a is not None else r'[[[\textit{free}]]]' for a in arg_values]) + ')'
                ret.append(latex_verbatim_block(r))
                ret.append(r'$\to$')
                ret.append(latex_verbatim_block(returned_rexpr.stylized_rexpr()))
                ret.append(r'\\')

            elif kind == 'memo_compute_start':
                (name, arity), arg_values = args
                r = name + '(' + ', '.join(
                    [str(a) if a is not None else r'[[[\textit{free}]]]' for a in arg_values]) + ')'
                ret.append(r'\textbf{Computing the memo for ' + latex_verbatim_block(r) + r'}:')
                ret.append(r'\begin{mdframed}[leftmargin=10,topline=false,bottomline=false,rightline=false]')
                #ret.append(r'\begin{leftbar}')
                #ret.append(r'\begin{longtable}{|l}')
            elif kind == 'memo_compute_end':
                #ret.append(r'\end{leftbar}')
                ret.append(r'\end{mdframed}')
                ret.append(r'\pagebreak')
                #ret.append(r'\end{longtable}')


        if rewrites.user_defined_rewrites_memo:
            ret.append(r'\textbf{The Memoization tables after all the rewrites have been completed (\cref{sec:memoization})}: \\')
            ret.append(r'\begin{longtable}{ccc}')
            for (name, arity), rexpr in rewrites.user_defined_rewrites_memo.items():
                r = name + '(' + ', '.join([color('vargreen', f'$arg_{i}') for i in range(arity)]) + ')'
                ret.append(r'\begin{minipage}{.3\textwidth}')
                ret.append(latex_verbatim_block(r))
                ret.append(r'\end{minipage} & $\to$ & \begin{minipage}{.5\textwidth}')
                ret.append(latex_verbatim_block(rexpr.stylized_rexpr()))
                ret.append(r'\end{minipage} \\')
            ret.append(r'\end{longtable}')

        return '\n'.join(ret)
    finally:
        color_output_latex = False
        generating_latex_output = False


if logging_rewrites:
    logging_rewrite_output = open(os.environ.get('REWITE_LOG', 'rewrite.log'), 'w+')
    def generate_latex_file():
        with open(os.environ.get('REWITE_LOG', 'rewrite.log') + '.tex', 'w+') as f:
            f.write(generate_latex())

    #atexit.register(generate_latex_file)

indent_nested_amount = ' '

####################################################################################################

def get_exposed_variables(rexpr):
    if isVariable(rexpr):
        return {rexpr}
    elif not isinstance(rexpr, Term):
        return set()
    elif rexpr.name == 'proj' and rexpr.arity == 2:
        c = get_exposed_variables(rexpr.get_argument(1))
        return c - {rexpr.get_argument(0)}
    elif rexpr.name == 'aggregator' and rexpr.arity == 4:
        c = get_exposed_variables(rexpr.get_argument(3))
        return c - {rexpr.get_argument(2)} | {rexpr.get_argument(1)}
    elif rexpr.name == 'if' and rexpr.arity == 3:
        return get_exposed_variables(rexpr.get_argument(1)) | get_exposed_variables(rexpr.get_argument(2))
    else:
        c = set()
        for a in rexpr.arguments:
            c |= get_exposed_variables(a)
        return c



####################################################################################################
# base definitions of Term structure.
# Every term has a name and a variable number of arguments

class Term:

    if not logging_rewrites:
        __slots__ = (
            # the base methods which are used for actually representing the term
            ('__name', '__arguments', '__hashcache'))

        #     # a bunch of extra "junk" that we attach to the terms for printing them out into the nice looking form
        #     ('_debug_active_rewrite', '_debug_constructed_from', '_debug_unique_id', '_debug_term_size',
        #      '_debug_printer_controller', '_debug_identified_as_same',
        #      '_debug_should_instead_print',
        #     )
        #     #if logging_rewrites else ())
        # )

    def __init__(self, name, arguments):
        #assert all(not isinstance(a, Variable) for a in arguments) and isinstance(name, str)  # there is not a special symbol for variables in this version
        assert isinstance(name, str)
        self.__name = name
        self.__arguments = tuple(arguments)  # ensure this is a tuple and thus immutable
        self.__hashcache = hash(self.name) ^ hash(self.__arguments)

        if logging_rewrites:
            self._debug_active_rewrite = active_rewrite.copy()
            self._debug_constructed_from = []
            self._debug_unique_id = f'gened_term:{self.name}_{self.arity}_{uuid.uuid4().hex}'  # make a unique id for this
            s = 0
            for a in self.arguments:
                if isinstance(a, Term): s += a._debug_term_size
                else: s += len(str(a))
            self._debug_term_size = len(self.name) + s + len(self.arguments)*2 + 2  # a rough estimate of how big this term is when printing

        # if self.name == '+':
        #     arg_sets = [get_exposed_variables(a) for a in self.arguments]
        #     assert all(arg_sets[0] == a for a in arg_sets)

    @property
    def arity(self):
        return len(self.__arguments)

    @property
    def name(self):
        return self.__name

    @property
    def arguments(self):
        return self.__arguments

    def get_argument(self, idx):
        return self.__arguments[idx]

    def __iter__(self):
        yield self.__name
        yield from self.__arguments

    def __hash__(self):
        return self.__hashcache

    def __eq__(self, other):
        return self is other or \
            (isinstance(other, Term) and \
             self.__hashcache == other.__hashcache and \
             self.__name == other.__name and \
             self.__arguments == other.__arguments)

    def __str__(self):
        return self.stylized_rexpr()
        return self.name + '(' + ', '.join(map(str, self.arguments)) + ')'

    def __repr__(self): return str(self)

    def indented_str(self, indent=0):
        if self.name == '$VARIABLE' and self.arity == 1:
            return ' '*indent + f'$VARIABLE({self.arguments[0]})'
        ret = [' '*indent + self.name]
        if self.arity > 0:
            ret.append('(\n')
        for a in self.arguments:
            if hasattr(a, 'indented_str'):
                ret.append(a.indented_str(indent+1))
                ret.append(',\n')
            else:
                ret.append(' '*(indent + 1))
                ret.append(str(a))
                ret.append(',\n')
        if self.arity > 0:
            ret.append(' '*indent + ')')
        return ''.join(ret)

    def style_raw_latex(self):
        return latex_verbatim_block(self.stylized_rexpr())

    def stylized_rexpr(self):
        print_control = getattr(self, '_debug_printer_controller', None)
        if print_control == 'NO' and limit_printed_amount and not isVariable(self):
            if generating_latex_output:
                return r'[[[$\cdots$]]]'
            return '...'  # omit this item
        return self.stylized_rexpr_orig()

    def stylized_rexpr_orig(self):
        # return a string for this which is supposed to be close to representtaion used in the paper as possible

        def nested(r):
            # print out a nested R-expr, if the expression is big, then it will indent, otherwise it will try and make it inline
            if isinstance(r, str):
                return '"' + r.replace('"', '\\"').replace('\n', '\\n') + '"'
            elif isinstance(r, float):
                return f'{r:.3f}'
            elif isinstance(r, Term):
                s = r.stylized_rexpr()
            else:
                s = str(r)
            s = s.strip()
            if '\n' in s or len(s) > 50:
                s = '\n' + indent(s, indent_nested_amount) + '\n'
            return s

        # def nested_value(r):
        #     return nested(r).strip()


        def nested_p(ro):
            r = nested(ro)
            if isinstance(ro, Term) and ro.name == '+':  # the order of operations might not match what the syntax tree should print out, so this will check for that
                return '('+r.strip()+')'
            return r.rstrip()

        if generating_latex_output and self.name == '...' and self.arity == 0:
            assert False  # I don't think that this is used anymore
            import ipdb; ipdb.set_trace()
            return r'[[[$\cdots$]]]'  # this is ommited
        elif generating_latex_output and isMultiplicity(self):
            # then generate some escape sequence for the multiplicity
            if self.get_argument(0) == float('inf'):
                return '[[[$\overline{\infty}$]]]'
            return r'[[[$\overline{\texttt{'+str(self.get_argument(0)) + r'}}$]]]'  # this is going to cause the width of the character to change....
        elif self.name == '$VARIABLE' and self.arity == 1:
            v = self.get_argument(0)
            if isinstance(v, int):
                # becuase ints are used for the arguments, they can be hard to
                # see in the argument place, so need something more in the
                # representation here
                v = f'$ARG_{v}'
            v = str(v)
            if v.startswith('$VAR_'):
                # then convert this into a number and then represent this as a number such that it will be printable as a variable
                i = int(v[5:])
                assert i > 0
                ret = ''
                letters = 'ABCDEFGHJKLMNTUVW'  # the letters IOQRSXYZP are removed
                while i != 0:
                    ret += letters[i % len(letters)]
                    i = (i - (i % len(letters))) // len(letters)
                return color('vargreen', ret) # the fastest moving part will be first in the string, which means that it should be easier to read
            else:
                return color('vargreen', str(v).capitalize())
        elif self.name == '=' and self.arity == 2:
            return f'({nested(self.get_argument(0)).strip()}={nested(self.get_argument(1)).strip()})'
        elif self.name == 'aggregator' and self.arity == 4:
            return (
                '('+color('aggblue', f'{nested(self.get_argument(1))}={self.get_argument(0)}({nested(self.get_argument(2))},')+
                f'{nested(self.get_argument(3))}))')
        # elif self.name == 'structure':
        #     return f'({nested(self.get_argument(1))}={nested(self.get_argument(0))}(' + ', '.join(map(nested, self.arguments[2:])) + '))'
        elif self.name in ('+', '*'):
            if generating_latex_output:
                return (self.name + r'[[[\allowbreak ]]]').join(map(nested_p,self.arguments))
            return self.name.join(map(nested_p,self.arguments))
        elif self.name == 'proj' and self.arity == 2:
            # then this is a projection expression, if the nested expression is
            # also a projection, then we we are not going to nest the expression
            # so we will get something like proj(A, proj(B,  stuff ))
            var, body = self.arguments
            var = nested(var)
            if isinstance(body, Term) and body.name == 'proj' and body.arity == 2:
                bodyv = body.stylized_rexpr().strip()
                return f'proj({var}, {bodyv})'
            else:
                bodyv = nested(body)
                return f'proj({var}, {bodyv})'
        else:
            # this covers all base cases and expressions like if which are just represented via their name
            args = [nested(a) for a in self.arguments]
            for i in range(len(args)-1):
                s = args[i].lstrip()  # remove any new lines at the start
                # this needs to add a comma, but would like to do it before any white space at the end
                j = len(s) - 1
                while j > 0 and s[j] in '\n\t ':
                    j -= 1
                s = s[0:j+1] + ', ' + s[j+1:]
                args[i] = s
            if args:
                args[-1] = args[-1].lstrip()

            return f'{self.name}(' + ''.join(args) + ')'   #', '.join(map(lambda s: s.strip(), map(nested, self.arguments))) + ')'


def Variable(name):
    if isinstance(name, Term) and name.name == '$VARIABLE':
        return name
    return Term('$VARIABLE', (name,))

def isVariable(x):
    return isinstance(x, Term) and x.name == '$VARIABLE' and x.arity == 1


class _multiplicityTerm(Term):
    def __init__(self, val):
        assert (isinstance(val, int) and val >= 0) or val == float('inf')
        super().__init__('$MUL', (val,))
    def __add__(self, other):
        if not isinstance(other, (int, float)):
            assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
            other = other.arguments[0]
        return type(self)(self.arguments[0] + other)
    def __mul__(self, other):
        if not isinstance(other, (int, float)):
            assert isinstance(other, Term) and other.name == '$MUL' and other.arity == 1
            other = other.arguments[0]
        a = self.arguments[0]
        # if either value is inf, then having 0*inf == nan, but we would like it to be 0
        if a == 0 or other == 0:
            return type(self)(0)
        return type(self)(a * other)
    __rmul__ = __mul__  # the direction of this does not matter, but it might be called with an int on the lhs
    __radd__ = __add__
    def __int__(self): return self.arguments[0]
    def __eq__(self, other):
        if isinstance(other, (int, float)):
            return other == self.arguments[0]
        return super().__eq__(other)
    def __hash__(self): return super().__hash__()

def multiplicity(val):
    if isinstance(val, int):
        return _multiplicityTerm(val)
    if isinstance(val, _multiplicityTerm):
        return val
    if val == float('inf'):
        #import ipdb; ipdb.set_trace()
        return _multiplicityTerm(val)
    assert False  # wtf

def isMultiplicity(val):
    return isinstance(val, _multiplicityTerm)

####################################################################################################


generated_var_cnt = 0
def generate_var():
    global generated_var_cnt
    generated_var_cnt += 1
    assert generated_var_cnt > 0
    return Variable(f'$VAR_{generated_var_cnt}')

@track_rexpr_constructed
def uniquify_variables(rexpr, mapping=None):
    if not isinstance(rexpr, Term):
        return rexpr
    if mapping is None: mapping = {}
    return uniquify_variables_rec(rexpr, mapping)


def uniquify_variables_rec(rexpr, mapping):
    if not isinstance(rexpr, Term):
        return rexpr
    if isVariable(rexpr):
        # if the variable is contained in the expression, then this is going to give it a new name
        return mapping.get(rexpr, rexpr)
    elif rexpr.name == 'proj' and rexpr.arity == 2:
        var = rexpr.get_argument(0)
        rxp = rexpr.get_argument(1)
        new_var = generate_var()
        old = mapping.get(var)
        mapping[var] = new_var
        rxp = uniquify_variables_rec(rxp, mapping)
        mapping[var] = old
        return Term('proj', (new_var, rxp))
    elif rexpr.name == 'aggregator' and rexpr.arity == 4:
        op, resulting, incoming, rxp = rexpr.arguments
        new_var = generate_var()
        assert resulting != incoming  # this needs to be handled differently
        resulting = mapping.get(resulting, resulting)  # do the remapping for the returned variable
        old_var = mapping.get(incoming)
        mapping[incoming] = new_var
        rxp = uniquify_variables_rec(rxp, mapping)
        mapping[incoming] = old_var
        return Term('aggregator', (op, resulting, new_var, rxp))
    else:
        # this should attempt to rewrite all of the arguments of the term
        ret = []
        for v in rexpr.arguments:
            ret.append(uniquify_variables_rec(v, mapping))
        ret = Term(rexpr.name, ret)
        if ret != rexpr:
            return ret
        else:
            # avoid duplicating this if there are no changes
            return rexpr


def walk_rexpr(rexpr, func):
    # match against the R-exprs which have nested expressions
    def w(r):
        func(r)
        if r.name in ('+', '*'):
            for a in r.arguments:
                w(a)
        elif r.name == 'proj' and r.arity == 2:
            w(r.get_argument(1))
        elif r.name == 'aggregator' and r.arity == 4:
            w(r.get_argument(3))
        elif r.name == 'if' and r.arity == 3:
            for i in range(3):
                w(r.get_argument(i))
    w(rexpr)

def contains_variable(rexpr, var):
    assert isVariable(var)
    if not isinstance(rexpr, Term):
        return False
    if rexpr == var:
        return True
    if isinstance(rexpr, Term):
        for a in rexpr.arguments:
            if contains_variable(a, var): return True
    return False

def contains_any_variable(rexpr):
    if isVariable(rexpr): return True
    if isinstance(rexpr, Term):
        for a in rexpr.arguments:
            if contains_any_variable(a): return True
    return False

def replace_term(expr, mapping):
    if expr in mapping:
        return mapping[expr]
    elif isinstance(expr, Term):
        ret = []
        did_change = False
        for a in expr.arguments:
            n = replace_term(a, mapping)
            if n != a: did_change = True
            ret.append(n)
        if did_change:
            return Term(expr.name, ret)
        return expr  # return unmodified if nothing changes
    else:
        return expr

def filter_out_variable(rexpr, variable):
    # if the term contains the variable, then just remove it from the expression
    # if not isinstance(rexpr, Term):
    #     return rexpr
    if rexpr.name == 'aggregator' and rexpr.arity == 4:
        if rexpr.get_argument(1) == variable or rexpr.get_argument(2) == variable:
            return multiplicity(1)
        r = filter_out_variable(rexpr.get_argument(3), variable)
        if r != rexpr.get_argument(3):
            return Term('aggregator', (rexpr.get_argument(0), rexpr.get_argument(1), rexpr.get_argument(2), r))
        return rexpr
    elif rexpr.name == 'proj' and rexpr.arity == 2:
        assert rexpr.get_argument(0) != variable  # this should be projected out, so this should not be used for this
        r = filter_out_variable(rexpr.get_argument(1), variable)
        if r != rexpr.get_argument(1):
            return Term('proj', (rexpr.get_argument(0), r))
        return rexpr
    elif rexpr.name == '*':
        return make_conjunction(*(filter_out_variable(r, variable) for r in rexpr.arguments))
    elif rexpr.name == '+':
        return make_disjunction(*(filter_out_variable(r, variable) for r in rexpr.arguments))
    elif rexpr.name == 'if' and rexpr.arity == 3:
        return Term('if', (filter_out_variable(r, variable) for r in rexpr.arguments))
    elif contains_variable(rexpr, variable):
        return multiplicity(1)
    return rexpr

class IdentityWrapper:
    def __init__(self, v): self.v = v
    def __eq__(self, o): return isinstance(IdentityWrapper, o) and self.v is o.v
    def __hash__(self): return id(self.v)

def replace_identicial_term(expr, mapping):
    # there needs to be a method for replacing an expression which is a particular instances, using the IS expression
    # though if something is duplicated more than once in the expression (as these are read only pointers already), then that might
    # cause a particular problem?  I suppose that this would require a particular path through the expression, or we could just
    # make local copies throughout the expression such that unified expressions are distinct in memory
    i = IdentityWrapper(expr)
    if i in mapping:
        return mapping[i]
    elif isinstance(expr, Term):
        ret = []
        did_change = False
        for a in expr.arguments:
            n = replace_identicial_term(a, mapping)
            if n is not a: did_change = True
            ret.append(n)
        if did_change:
            return Term(expr.name, ret)
        return expr  # return unmodified if nothing changes
    else:
        return expr


def gather_branches(rexpr, *, through_nested=True):
    if not isinstance(rexpr, Term):
        return
    if rexpr.name == '+':
        yield rexpr
    if not through_nested and ((rexpr.name == 'proj' and rexpr.arity == 2) or (rexpr.name == 'aggregate' and rexpr.arity == 4)):
        # in this case, we are not going to want to pass through nested projection or aggregators.
        return
    if rexpr.name == '*':
        for a in rexpr.arguments:
            yield from gather_branches(a, through_nested=through_nested)
    if rexpr.name == 'aggregator' and rexpr.arity == 4:
        yield from gather_branches(rexpr.get_argument(3), through_nested=through_nested)
    if rexpr.name == 'proj' and rexpr.arity == 2:
        yield from gather_branches(rexpr.get_argument(1), through_nested=through_nested)

def gather_environment(rexpr):
    if not isinstance(rexpr, Term):
        return
    # or rexpr.name == '+' or isVariable(rexpr):
    #     return
    # if rexpr.name == 'if' and rexpr.arity == 3:
    #     # we do not know which branch of an if expression is going to run yet, so it is like a disjunction in that way
    #     return
    yield rexpr
    if rexpr.name == 'proj' and rexpr.arity == 2:
        for r in  gather_environment(rexpr.get_argument(1)):
            if not contains_variable(r, rexpr.get_argument(0)):
                yield r
    # in the case that the body of an aggregator goes to zero, it will just result in the identity element
    # which does not mean there is some conjunction between the different values
    # elif rexpr.name == 'aggregator' and rexpr.arity == 4:
    #     yield from gather_environment(rexpr.get_argument(3))
    elif rexpr.name == '*':
        for a in rexpr.arguments:
            yield from gather_environment(a)


####################################################################################################
# The core of the rewriting engine and pattern matching
# These classes are designed to be "mostly" general in that they do not specialize to specific rewrites
# These do implement the R-expr rewrites which


class UnificationFailure(Exception):
    # def __init__(self):
    #     super().__init__()
    #     import ipdb; ipdb.set_trace()
    pass

class RewriteContext(set):
    """This is the context in which a given rewrite is being performed.  This can
    be though of as an (incomplete) set of conjunctive R-exprs to the current
    R-expr.  There are methods which are designed to assist with looking up
    expressions more efficiently and retrieving the assignment of a variable
    """

    def __init__(self, parent=None, *, set_vals=None):
        super().__init__()
        self._parent = parent
        self._assign_index = {}
        self._unifies_index = defaultdict(set)  # this is currently not used for anything
        self._kind_index = defaultdict(set)
        self._argument_index = defaultdict(set)
        self._variable_type_index = {}

        if set_vals is not None:
            for s in set_vals:
                self.add_rexpr(s)

        # if set_vals is not None:
        #     super().__init__(set_vals)
        #     self._build_indexes()
        # else:
        #     super().__init__()
        # # self._multiplicity = 1  # a tracker on the multiplicity of the
        # #                         # expression so things get multiplied together

    def _assert_variable_type(self, var, struct):
        r = self.get_variable_type(var)
        if r is not None:
            if r != (struct.name, struct.arity):
                raise UnificationFailure()
        else:
            self._variable_type_index[var] = (struct.name, struct.arity)

    def _index_unify_rexpr(self, r):
        a,b = r.arguments
        # if a in self._variable_type_index:
        #     import ipdb; ipdb.set_trace()
        if isVariable(a) and isVariable(b):
            # then this is a unification between two variables with neither ground
            # if either of these are set, then the other one should also become set
            av, bv = self.get_value(a), self.get_value(b)
            if av is None and bv is None:
                # then this is just a unification between these two variables
                self._unifies_index[a].add(b)
                self._unifies_index[b].add(a)
            elif av is None:
                # then assign a the value of the variable b
                self._set_variable(a, bv)
            elif bv is None:
                self._set_variable(b, av)
            elif av != bv:
                # both are assigned, but they are not values that unify together
                raise UnificationFailure()
        else:
            if isVariable(b) and not isVariable(a):
                # swap a and b so that a is always the variable
                a,b = b,a
            if isVariable(a) and isinstance(b, Term):
                self._assert_variable_type(a, b)
            av = self.get_value(a)
            if av is not None:
                if av != b:
                    if contains_any_variable(b):
                        # then this contains free vraiables, which means that this should do unification on the new added expression
                        assert isinstance(b, Term)
                        if not isinstance(av, Term) or av.name != b.name or av.arity != b.arity:
                            raise UnificationFailure()
                        for avv, bv in zip(av.arguments, b.arguments):
                            self.add_rexpr(Term('=', (avv,bv)))
                    else:
                        # the value of b does not match
                        raise UnificationFailure()
            elif contains_any_variable(b):
                # this means that it is an expression like (A=f(X,Y,Z)) which
                # means that the rhs still contains variables, so there is
                # nothing to propagate here save the result of this assignment
                # in the index
                typ = self.get_variable_type(a)
                if typ is not None and (b.name, b.arity) != typ:
                    raise UnificationFailure()
                self._variable_type_index[a] = (b.name, b.arity)
                for ot in self._unifies_index[a]:
                    if not isVariable(ot):
                        # then this is a term structure that this unifies with, if the name does not match, then we should throw
                        if ot.name != b.name or ot.arity != b.arity:
                            raise UnificationFailure()
                self._unifies_index[a].add(b)
            else:
                self._set_variable(a, b)

    def _set_variable(self, var, val):
        if isVariable(var):
            cv = self.get_value(var)
            if cv is not None:
                if cv != val:
                    raise UnificationFailure()
            else:
                assert not contains_any_variable(val)
                #print(f'Variable assigned {var}={val}')
                self._assign_index[var] = val
                # set the value of all variables that this variable is unified with
                # so this is eager propagating in the context for unification of constants
                # which is maybe a slight difference from what is written in the paper currently?
                for unified in self._get_unified(var):
                    self._set_variable(unified, val)
        else:
            assert isinstance(var, Term)
            # this is a nested structure term
            if not isinstance(val, Term) or var.name != val.name or var.arity != val.arity:
                # this means that the unified expressions for these two does not match
                raise UnificationFailure()
            else:
                # this should attempt to make the two expressions unify together
                for a, b in zip(var.arguments, val.arguments):
                    self.add_rexpr(Term('=', (a,b)))

    def _get_unified(self, var):
        if self._parent is not None:
            yield from self._parent._get_unified(var)
        if var in self._unifies_index:
            yield from self._unifies_index[var]


    def add_rexpr(self, r):
        assert isinstance(r, Term)
        if self.__contains__(r):
            # this is already tracked, sowe are not going to add it again to the expression
            return

        if isMultiplicity(r):
            # we ignore tracking of multiplicies here as this is just a _set_
            # these are returned else where such that it will find
            return

        for a in r.arguments:
            # track that this argument appeared in this expression
            self._argument_index[a].add(r)

        if r.name in ('+', '*') or (r.name, r.arity) in (('proj', 2), ('aggregate', 4), ('if', 3)):
            # ignore expressions which are the disjunctions/conjunctions themselves.
            # These are nested expressions
            return

        # so that we can find things like lessthan(A,B) by just looking under lessthan/2
        self._kind_index[(r.name, r.arity)].add(r)
        super().add(r)  # this is just tracking the standard r-expr term

        if r.name == '=' and r.arity == 2:
            self._index_unify_rexpr(r)

    # override the set method to also add rexprs
    add = add_rexpr

    def __contains__(self, v):
        if super().__contains__(v):
            return True
        if self._parent is not None:
            return self._parent.__contains__(v)
        return False

    # for dealing with recursive operations, this will introduce a new copy and
    # then use set intersection and subtraction to extract the relevant parts of
    # the expression
    def copy(self, constructing_rexpr=None):
        r = RewriteContext(self)
        r._constructing_rexpr = constructing_rexpr
        return r

    def __and__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        # this is the things which are shared between both of these expressions
        # if there is something which
        if self is o:
            # this would need to be a copy, so it would want to copy itself
            return self

        # this is going to rebuild the index which is "slow" but should still be ok with this working
        res = RewriteContext(parent=self._parent, set_vals=super().__and__(o))

        for var, typ in set(self._variable_type_index.items()) & set(o._variable_type_index.items()):
            # this should invent some dummy expression for this to track the type of this variable
            #import ipdb; ipdb.set_trace()
            res.add_rexpr(Term('=', (var, Term(typ[0], (Variable(('internal_dummy', object())) for _ in range(typ[1]))))))

        return res

    def __sub__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        # if this is subtracting one thing from another, then the parent should not be included
        if self is o:
            # tihs is going to have to rewrite an empty environment as these are equivalent or something?
            return RewriteContext()

        if bool(o) is False:
            # then the other thing is empty, so we can just return ourselves without changes
            return self

        # this will make a blank copy of ourselves, and then identify what is not getting removed
        # those items will then be added to the new copy
        if self._parent is not None:
            ns = self._parent.copy()
        else:
            ns = RewriteContext()
        for a in self.iter_all():
            if a not in o:
                ns.add(a)

        return ns

        # import ipdb; ipdb.set_trace()
        # assert False

    # don't use the inplace expressions for now, though these might make it more efficient in the future?
    def __iand__(self, o):
        assert isinstance(o, RewriteContext) and o._parent is self._parent
        if self is o:
            # this is itself, there is no modification that is required
            return self
        return NotImplemented

    # def __isub__(self, o):
    #     assert isinstance(o, RewriteContext) and o._parent is self._parent
    #     if bool(
    #     if self is o:
    #         # this is going to have to return an empty value, which means that
    #         return NotImplemented
    #     return NotImplemented

    def get_value(self, variable):
        if isVariable(variable):
            r = self._assign_index.get(variable)
            assert not contains_any_variable(r)
            if r is None and self._parent is not None:
                return self._parent.get_value(variable)
            return r
        else:
            assert not contains_any_variable(variable)
            # this is not a variable, so just return the value back.  if there is nothing here,
            return variable

    def get_variable_type(self, variable):
        if self._parent is not None:
            r = self._parent.get_variable_type(variable)
            if r is not None:
                return r
        return self._variable_type_index.get(variable)

    def get_all_kinds(self, name, arity):
        r = self._kind_index.get((name, arity), set())
        if self._parent is not None:
            r |= self.get_all_kinds(name, arity)
        return r

    def iter_local(self):
        return super().__iter__()

    def iter_all(self):
        if self._parent is not None:
            yield from self._parent.iter_all()
        yield from self.iter_local()

    def local_to_rexpr(self):
        # this is going to have to take all of the local conjunctive information and turn it into a R-expr which can
        res = []
        for var, val in self._assign_index.items():
            var_name = var.get_argument(0)
            if not contains_any_variable(val) and not (isinstance(var_name, tuple) and var_name[0] == 'internal_dummy'):  # otherwise this is internal
                res.append(Term('=', (var, val)))  # this is just the assignments to ground variables, all of the other expressions should still be in the R-expr
        for var, typ in self._variable_type_index.items():
            if self._parent is None or self._parent.get_variable_type(var) != typ:
                res.append(Term('enforce_type', (var, typ[0], typ[1])))

        # if self._variable_type_index:
        #     import ipdb; ipdb.set_trace()
        #assert '<object object' not in str(res)
        r = make_conjunction(*res)  # this is just a conjunction of the constraints which are present
        assert 'internal_dummy' not in str(r)
        return r

    def get_associated_with_var(self, var):
        for r in self._get_associated_with_var(var):
            if 'internal_dummy' not in str(r):  # this is such a hacky way to do this.....
                yield r

    def _get_associated_with_var(self, var):
        assert isVariable(var)
        if self._parent is not None:
            yield from self._parent._get_associated_with_var(var)
        if var in self._argument_index:
            yield from self._argument_index[var]

    def update(self, child):
        assert child._parent is self
        # this neds to go through and update ourselves with everything from the child
        for c in child.iter_local():
            self.add(c)
        for var, typ in child._variable_type_index.items():
            assert self._variable_type_index[var] == typ

    def update_except(self, child, not_include_variable):
        assert child._parent is self
        old = self._argument_index.get(not_include_variable)
        if old is not None: old = old.copy()  # make a copy as we want to ensure it is not modified
        dummy_variable_name = Variable(('internal_dummy', object()))  # if this is used, then it will keep around
        for c in child.iter_local():
            # this is giong to take everything from the child which does not mention the variable
            if not contains_variable(c, not_include_variable):
                self.add_rexpr(c)
            elif (c.name == '=' and c.arity == 2
                  and not contains_variable(c.get_argument(0), not_include_variable)
                  and contains_variable(c.get_argument(1), not_include_variable)):
                z = replace_term(c, {not_include_variable: Variable(dummy_variable_name)})
                self.add_rexpr(z)
                #import ipdb; ipdb.set_trace()
                # this could construct new temp variable variable names for the rhs, which would then allow this representation to stay around
                # but those temp variable names would not be associated with the expression anymore?
        for var, typ in child._variable_type_index.items():
            if var != not_include_variable:
                c = self._variable_type_index.get(var)
                if c != typ:
                    assert c is None
                    self._variable_type_index[var] = typ
                #assert self._variable_type_index[var] == typ

        assert self._argument_index.get(not_include_variable) == old


    # def __del__(self):
    #     a = False
    #     for var, typ in self._variable_type_index.items():
    #         if not self._parent.get_variable_type(var):
    #             a = True
    #     if a:
    #         import ipdb; ipdb.set_trace()


class RewriteCollection:
    """A class for tracking rewrite operators.  Operators are segmented such that
    they can be looked up quickly using the name of an R-expr or in the case
    that more advanced matching is required

    """
    def __init__(self):
        # these only contain the "fast" rewrites, which is matching on the name or name/arity
        self.name_match = defaultdict(list)
        self.name_arity_match = defaultdict(list)

        # these are additional rewrites which should also consider
        self.name_match_fulls = defaultdict(list)
        self.name_arity_match_fulls = defaultdict(list)
        self.full_match = defaultdict(list)

        # this would be some one-to-one matching of an expression
        # these are not "functions" which correspond with builtins, but rather R-exprs
        # that are used to replace the given expression
        self.user_defined_rewrites = {}

        self.user_defined_rewrites_memo = {}

        self.user_defined_rewrites_used = {}  # when a rewrite is used, add it here so that we know what to print

        self.memo_version_used = 2

        self.rewrite_used_count = defaultdict(int)

    def _register_function(self, pattern, func, kind='fast'):
        # determine which of the patterns are required for a given expression
        if not hasattr(func, '_matching_pattern'):
            func._matching_pattern = []
        patterns = parse_sexp(pattern)
        func._matching_pattern += patterns
        if func not in self.rewrite_used_count:
            self.rewrite_used_count[func] = 0
        for pattern in patterns:
            any_arity = False
            complex_pattern = False
            arity = None
            name, *args = pattern
            assert isinstance(name, str)
            for i, v in enumerate(args):
                if isinstance(v, tuple): complex_pattern = True # there is more here to match
                elif v == 'any': any_arity = True
                elif v == 'args': arity = int(args[i+1])
            if complex_pattern:
                self.full_match[pattern].append(func)
            elif any_arity:
                if kind == 'fast':
                    self.name_match[name].append(func)
                else:
                    self.name_match_fulls[name].append(func)
            else:
                if arity is None:
                    arity = len(args)
                if kind == 'fast':
                    self.name_arity_match[(name, arity)].append(func)
                else:
                    self.name_arity_match_fulls[(name, arity)].append(func)

    def register_function(self, pattern, kind='fast'):
        def f(func):
            self._register_function(pattern, func, kind)
            return func
        return f

    def do_user_defined_rewrite(self, rewrite_engine, rexpr):
        # this should take the name arity of a given expression and then subsuite in new names for the variables
        n = (rexpr.name, rexpr.arity)
        # if n == ('in', 2) and 'DB' in str(rexpr):
        #     import ipdb; ipdb.set_trace()
        self.user_defined_rewrites_used[n] = True
        if n in self.user_defined_rewrites_memo:
            return self.do_access_memo(rewrite_engine, rexpr)
        rxp = self.user_defined_rewrites[n]
        # this will have to subsuite in the variable names and create new variable names for the expression
        var_map = {}
        for i, new_name in enumerate(rexpr.arguments):
            var_map[Variable(i)] = new_name

        # this will have to replace all of the variables or create new variable names for all of the expression
        rxp = uniquify_variables(rxp, var_map)

        rxp = track_constructed(rxp, 'rr:user_defined_rewrite',
                                r'The user defined function \rterm{' f'{rexpr.name}/{rexpr.arity}' r'} is looked up.  '
                                r'All new variables are given new unique names.',
                                rexpr)

        # we DO NOT immediatly apply rewrites to the returned expression as that could cuase recursive programs to run forever

        # TODO: the depth lmiting is not included in this version currently.  This will have to walk through the R-expr
        # and identify user calls and add a call depth as some extra hidden meta data on the parameter or something

        return rxp

    def do_access_memo(self, *args):
        return [
            None,
            self.do_access_memo_version1,
            self.do_access_memo_version2
        ][self.memo_version_used](*args)

    def do_access_memo_version2(self, rewrite_engine, rexpr):
        n = (rexpr.name, rexpr.arity)
        rxp = self.user_defined_rewrites_memo[n]

        local_simplify = RewriteEngine(rewrites=self)

        arg_values = []
        for arg in rexpr.arguments:
            if isVariable(arg):
                val = rewrite_engine.context.get_value(arg)  # this will return None if the value is not set yet
            else:
                val = arg
                assert not contains_any_variable(val)
            arg_values.append(val)

        # add to the context the current value of these variables
        for i, val in enumerate(arg_values):
            if val is not None:
                local_simplify.context.add_rexpr(Term('=', (Variable(i), val)))

        # identify if the expression is contained in the memo table
        with no_logging_rewrites():
            res_rexpr = local_simplify.rewrite_fully(rxp, add_context=False)
        if match(rewrite_engine, res_rexpr, '($compute_fallback any)'):
            # then this has to fallback to whatever compute has for these given values
            # this will need to replace the expression in the memoized R-expr with the new expression.

            if rewrite_engine.should_defer_computing_memo(rexpr, arg_values):
                # an explicit defer has been requested by something else
                return rexpr

            new_contains_rexpr = []
            memo_simplify = RewriteEngine(rewrites=self)
            for i, val in enumerate(arg_values):
                if val is not None:
                    r = Term('=', (Variable(i), val))
                    memo_simplify.context.add_rexpr(r)
                    new_contains_rexpr.append(r)

            original_rexpr = self.user_defined_rewrites[n]
            try:
                log_event('memo_compute_start', n, arg_values)
                # with no_logging_rewrites():
                memos_returned = memo_simplify.rewrite_fully(original_rexpr)
            finally:
                log_event('memo_compute_end')

            rxp = self.user_defined_rewrites_memo[n]  # reload the original expression as it might have changed
            # because we do not allow for self cycles in this paper, we know that the version which is contained in the memo table must
            # not have the current value.  If we were to have a cycle, then it is possible for this to occur

            # wrap the new memo in an if-expression which controlls if this reads the given value
            memo_access = Term('if', (
                make_conjunction(*new_contains_rexpr),
                memos_returned,
                res_rexpr
            ))

            rxp2 = replace_term(rxp, {res_rexpr : memo_access})

            # reoptimize the new expression in a new context to combine if expressions (if possible)
            optimize_memos_simplify = RewriteEngine(rewrites=self)
            with no_logging_rewrites():
                # this is not logging the rewrites of the nested memo operation, so tihs is not seeing the different expressions
                # if this were to represent that there is a siderule to show that this was inside of some nested context
                # that might be interesting to see how this would represent those expressions
                rxp2 = optimize_memos_simplify.rewrite_fully(rxp2)

            self.user_defined_rewrites_memo[n] = rxp2

            log_event('memo_computed', n, arg_values, memos_returned)

            # fall through to return memos_returned
        elif match(rewrite_engine, res_rexpr, '(if rexpr rexpr rexpr)'):
            # then this is the indeterminate case for the memo where it can not determine if the value is contained
            log_event('memo_indeterminate', n, arg_values)
            return rexpr
        else:
            # have to construct an expression which includes the assignment to the variables which might be stored in the context
            memos_returned = make_conjunction(local_simplify.context.local_to_rexpr(), res_rexpr)

            log_event('memo_looked_up', n, arg_values, memos_returned)

        var_map = {}
        for i, new_name in enumerate(rexpr.arguments):
            var_map[Variable(i)] = new_name

        memos_returned = uniquify_variables(memos_returned, var_map)

        return memos_returned

    def do_access_memo_version1(self, rewrite_engine, rexpr):
        n = (rexpr.name, rexpr.arity)

        rxp = self.user_defined_rewrites_memo[n]

        [[contains_memo,  memos, original_rexpr]] = match(rewrite_engine, rxp, '(if rexpr rexpr rexpr)')

        # make a new context for evaluating the memoized expression
        # we need to fully evaluate this before the results are returned
        #run_context = RewriteContext()

        local_simplify = RewriteEngine(rewrites=self)  # this will make a new context for matching the rewrites

        arg_values = []
        for arg in rexpr.arguments:
            if isVariable(arg):
                val = rewrite_engine.context.get_value(arg)  # this will return None if the value is not set yet
            else:
                val = arg
                assert not contains_any_variable(val)
            arg_values.append(val)

        # add to the context the current value of these variables
        for i, val in enumerate(arg_values):
            if val is not None:
                local_simplify.context.add_rexpr(Term('=', (Variable(i), val)))

        with no_logging_rewrites():
            contains_memo_result = local_simplify.rewrite_fully(contains_memo, add_context=False)

        # this needs to match against the contains_memo_result to determine if the if expression would be true or false
        # or under determined.  if it is underdetermined, then we will _avoid_ reading the memo for now.  Otherwise

        condition_res = None

        for _ in match(local_simplify, contains_memo_result, '(mul 0)'):
            # meaning that the false branches was selected
            condition_res = False
        for _ in match(local_simplify, contains_memo_result, '(any-disjunction (mul >= 1))'):
            condition_res = True

        if condition_res is None:
            # then this is indeterminate, so we are just going to return the original R-expr in this case
            log_event('memo_indeterminate', n, arg_values)
            return rexpr

        elif condition_res is True:
            # then this has identified that the R-expr that we are looking for is contained inside of the memo table
            # so we are going to rewrite that expression to select the branch that we are looking for
            memo_simplify = RewriteEngine(rewrites=self)
            for i, val in enumerate(arg_values):
                if val is not None:
                    r = Term('=', (Variable(i), val))
                    memo_simplify.context.add_rexpr(r)
            with no_logging_rewrites():
                memos_returned = memo_simplify.rewrite_fully(memos)

            log_event('memo_looked_up', n, arg_values, memos_returned)

        elif condition_res is False:
            # then this needs to take the original R-expr and construct a new memoized expression
            # though if nothing is set, then we might just defer for a white
            if rewrite_engine.should_defer_computing_memo(rexpr, arg_values):
                return rexpr
            new_contains_rexpr = []
            # there might be other stuff which was added to the context that we do not want to "contanimate" the values
            memo_simplify = RewriteEngine(rewrites=self)
            for i, val in enumerate(arg_values):
                if val is not None:
                    r = Term('=', (Variable(i), val))
                    memo_simplify.context.add_rexpr(r)
                    new_contains_rexpr.append(r)
            # this should add back in the context for this expression, so we do not have to duplicate that here
            with no_logging_rewrites():
                memos_returned = memo_simplify.rewrite_fully(original_rexpr)

            # this is going to have to re-read the values from the memo table, as this might recurse around and have been updated in the process
            # the paper currently _does not_ handle self cycles, so this version of the code is ok for what we are demonstrating
            contains_memo, memos, _ = self.user_defined_rewrites_memo[n].arguments

            new_condition = make_disjunction(contains_memo, make_conjunction(*new_contains_rexpr))
            new_memo = make_disjunction(memos, memos_returned)
            # construct a new if-expression which has the newly stored memos
            self.user_defined_rewrites_memo[n] = Term('if', (new_condition, new_memo, original_rexpr))

            log_event('memo_computed', n, arg_values, memos_returned)

        # this needs to do renaming on the variables to match the caller's context and make new variables for anything else that is introduced
        var_map = {}
        for i, new_name in enumerate(rexpr.arguments):
            var_map[Variable(i)] = new_name

        memos_returned = uniquify_variables(memos_returned, var_map)

        return memos_returned

    def set_memoized(self, name, arity, kind='none'):
        assert kind in ('none', 'unk')

        n = (name, arity)
        if kind == 'none':
            self.user_defined_rewrites.pop(n)
        elif kind == 'unk':
            # meaning that this will just wait until it finds something
            if self.memo_version_used == 1:
                self.user_defined_rewrites_memo[n] = Term(
                    'if', (
                        multiplicity(0),  # indicate that nothing is currently memoized
                        multiplicity(0),  # the memo table is also currently empty
                        self.user_defined_rewrites[n]  # the original R-expr
                    ))

            elif self.memo_version_used == 2:
                self.user_defined_rewrites_memo[n] = Term('$compute_fallback', (Variable(i) for i in range(arity)))

    def define_user_rewrite(self, name, arity, rexpr):
        var_map = {}
        for i in range(arity):  # this will ensure that these variables keep
                                # their old name rather than getting replaced
            v = Variable(i)
            var_map[v] = v
            assert contains_variable(rexpr, v)
        if not logging_rewrites:
            # becuase we want something that is nice to print, keeping the origional names should make this a little easier to read
            rexpr = uniquify_variables(rexpr, var_map)
        self.user_defined_rewrites[(name, arity)] = rexpr

        if (name,arity) in self.user_defined_rewrites_memo:
            # clear the current memos out and just reset them entirely
            self.set_memoized(name, arity, 'none')
            self.set_memoized(name, arity, 'unk')

    def get_matching_rewrites(self, rexpr, context=None):
        for x in self._get_matching_rewrites(rexpr, context):
            self.rewrite_used_count[x] += 1
            yield x

    def _get_matching_rewrites(self, rexpr, context=None):
        found_something = False
        # these full matches require that there is more contextual information for this
        # which means that this is going to be looking.
        # we might want to avoid doing the full matches most of the time, so this will need to be controllable
        # by some expression
        n = (rexpr.name, rexpr.arity)
        if context.kind == 'full':
            if rexpr.name in self.name_match_fulls:
                yield from self.name_match_fulls[rexpr.name]
            if n in self.name_arity_match_fulls:
                yield from self.name_arity_match_fulls[n]

            for pattern, funcs in self.full_match.items():
                if match(context, rexpr, pattern):
                    yield from funcs

        if rexpr.name in self.name_match:
            found_something = True
            yield from self.name_match[rexpr.name]

        if n in self.name_arity_match:
            found_something = True
            yield from self.name_arity_match[n]

        # in the case that the name/arity matches the user defined rewrite, then the generic user
        # rewrite handler will handle it
        if n in self.user_defined_rewrites:
            found_something = True
            if context.kind != 'full' and context.do_user_rewrites:
                # do not expand user defined rewrites when the is doing the "full" rewriting pass.  These is because the first rewrite that
                # fires will be the one that is selected, so some rewrites could get starved as a result
                yield self.do_user_defined_rewrite

        # if not found_something and not isMultiplicity(rexpr):
        #     import ipdb; ipdb.set_trace()
        assert found_something or isMultiplicity(rexpr) or rexpr.name == '$compute_fallback' # if we do not find some rewrite, then this is probably a typo with how the program was written

    def __str__(self):
        matches = []
        for name, vals in self.name_match.items():
            for val in vals:
                matches.append((f'{name}(...) -> {val.__name__}', val._matching_pattern))
        for (name, arity), vals in self.name_arity_match.items():
            args = ', '.join([f'${i}' for i in range(arity)])
            for val in vals:
                matches.append((f'{name}({args}) -> {val.__name__}', val._matching_pattern))
        for pattern, vals in self.full_match.items():
            for val in vals:
                matches.append((f'{pattern} -> {val.__name__}', val._matching_pattern))

        max_prefix_length = 0
        for a,b in matches:
            max_prefix_length = max(len(a), max_prefix_length)

        for i in range(len(matches)):
            a,b = matches[i]
            # this will render the matches for the builtin rewrites.  The length of the prefix is made the same so we can get the comments at the same point
            r = '  ' + a + ' '*(max_prefix_length - len(a)) + '     # '
            r += b[0]
            matches[i] = r

        user_matches = []
        for (name, arity), val in self.user_defined_rewrites.items():
            rexpr_str = val.stylized_rexpr()
            args = ', '.join([f'$arg_{i}' for i in range(arity)])
            mt = f'  {name}({args}) -> '
            rexpr_str = indent(rexpr_str, ' '*len(mt)).lstrip()
            user_matches.append(mt + rexpr_str)

        user_matches_memos = []
        for (name, arity), val in self.user_defined_rewrites_memo.items():
            rexpr_str = val.stylized_rexpr()
            args = ', '.join([f'$arg_{i}' for i in range(arity)])
            mt = f'  {name}({args}) -> '
            rexpr_str = indent(rexpr_str, ' '*len(mt)).lstrip()
            user_matches_memos.append(mt + rexpr_str)


        return ''.join(['RewriteCollection(\n',
                        '\n'.join(matches),
                        '\n',
                        '-' * 50,
                        '\n',
                        '\n'.join(user_matches),
                        '\nMemos: ', '-'*(50-7), '\n',
                        '\n'.join(user_matches_memos),
                        '\n)'])

    def __repr__(self): return str(self)

    def get_user_defined_rewrites_in_rexpr(self, rexpr):
        found = []
        def f(x):
            if (x.name, x.arity) in self.user_defined_rewrites:
                found.append(x)
        walk_rexpr(rexpr, f)
        return found


class RewriteEngine:
    """
    Recursively applies itself to the R-expr until it is rewritten
    """

    def __init__(self, *, rewrites: RewriteCollection=None, kind='fast', context=None, user_rewrites=True):
        self.__rewrites = rewrites or globals()['rewrites']
        assert kind in ('fast', 'full')
        self.__kind = kind
        self.__do_user_rewrites = user_rewrites

        # context can be modified as this moves through the different rewrites
        self.context = context or RewriteContext()

    # these are read only, so make access through a property
    @property
    def kind(self): return self.__kind

    @property
    def rewrites(self): return self.__rewrites

    @property
    def do_user_rewrites(self): return self.__do_user_rewrites

    @track_rexpr_constructed
    def apply(self, rexpr, *, top_disjunct_apply=False):
        old_context = self.context
        try:
            if top_disjunct_apply and self.kind == 'full':
                # then this will want to go through the expression and identify
                # which expressions this will know all of the conjunctive
                # constraints which are present.  Variable names should already
                # be made unique, so we don't have to concern ourselves with
                # proj/aggregate introducing new variables
                for e in gather_environment(rexpr):
                    self.context.add_rexpr(e)
            for func in self.rewrites.get_matching_rewrites(rexpr, self):
                res = func(self, rexpr)
                assert res is not None  # error as this means the implementation is incomplete
                if res != rexpr:
                    assert 'internal_dummy' not in str(res)
                    log_event('rewritten_result', rexpr, res)  # record what has happened such that we can do replacements in the generated code
                    self.context.add_rexpr(res)  # this is going to add the new R-expr to the context
                    return res  # stop trying to match the expression and accept this rewrite
            # no rewrite matched, so this is just going to return the R-expr unmodified
            self.context.add_rexpr(rexpr)  # still add to the environment to track
            return rexpr
        except UnificationFailure:
            #import ipdb; ipdb.set_trace()
            return track_constructed(multiplicity(0), (), 'An inconsistency was detected using the environment.', rexpr)
        finally:
            # this should be the same item
            assert old_context is self.context

    # def __call__(self, rexpr):
    #     return self.apply(rexpr)

    def rewrite_once(self, rexpr):
        old = rexpr
        rexpr = self.apply(rexpr)
        log_event('simplify_'+self.kind, old, rexpr)
        return make_conjunction(self.context.local_to_rexpr(), rexpr)

    def rewrite_fully_v1(self, rexpr, *, add_context=True):
        #while True:

        for _ in range(5):
            self.__kind = 'fast'  # first run fast rewrites
            for _ in range(2):
                # this contains the context values inside of itself
                old = rexpr
                rexpr = self.apply(rexpr, top_disjunct_apply=True)
                if old == rexpr: break
                log_event('simplify_fast', old, rexpr)
                #import ipdb; ipdb.set_trace()
            #break
            self.__kind = 'full'  # run the full rewrites to make this
            old = rexpr
            rexpr = self.apply(rexpr, top_disjunct_apply=True)
            if old == rexpr: break
            log_event('simplify_full', old, rexpr)

        if not add_context:
            return rexpr
        return make_conjunction(self.context.local_to_rexpr(), rexpr)

    def rewrite_fully_v3(self, rexpr, *, add_context=True):
        old_douser = self.__do_user_rewrites
        call_depth_limit = 8  # 8 is the min number for the neural example to work
        try:
            for loop_cnt in range(500): #while True:
                self.__kind = 'fast'
                while True:
                    old = rexpr
                    rexpr = self.apply(rexpr, top_disjunct_apply=True)
                    if old == rexpr: break
                    log_event('simplify_fast', old, rexpr)
                    call_depth_limit -= 1
                    if call_depth_limit <= 0:
                        self.__do_user_rewrites = False # stop inlining in new calls
                self.__kind = 'full'
                old = rexpr
                rexpr = self.apply(rexpr, top_disjunct_apply=True)
                if old == rexpr:
                    break
                log_event('simplify_full', old, rexpr)
            if not add_context:
                return rexpr
            return make_conjunction(self.context.local_to_rexpr(), rexpr)
        finally:
            self.__do_user_rewrites = old_douser


    def rewrite_fully(self, rexpr, *, add_context=True):
        old_douser = self.__do_user_rewrites
        call_depth_limit = 12
        try:
            for loop_cnt in range(500): #while True:
                self.__kind = 'fast'
                while True:
                    old = rexpr
                    rexpr = self.apply(rexpr, top_disjunct_apply=True)
                    if old == rexpr or isMultiplicity(rexpr): break
                    log_event('simplify_fast', old, rexpr)
                    self.__do_user_rewrites = False  # make this converge first with what it has
                if isMultiplicity(rexpr): break
                self.__kind = 'full'
                old = rexpr
                rexpr = self.apply(rexpr, top_disjunct_apply=True)
                if isMultiplicity(rexpr): break
                log_event('simplify_full', old, rexpr)
                if old == rexpr:
                    if old_douser:
                        self.__do_user_rewrites = True
                        self.__kind = 'fast'
                        rexpr = self.apply(rexpr, top_disjunct_apply=True)
                        if old == rexpr:
                            print('>>>>>>>>>>>>>>>>>', loop_cnt)
                            break
                        self.__do_user_rewrites = False
                        log_event('simplify_fast', old, rexpr)
                        call_depth_limit -= 1
                        if call_depth_limit <= 0: break
                    else:
                        print('>>>>>>>>>>>>>>>>>', loop_cnt)
                        break
            if not add_context:
                return rexpr
            return make_conjunction(self.context.local_to_rexpr(), rexpr)
        finally:
            self.__do_user_rewrites = old_douser

        # for _ in range(5):
        #     self.__kind = 'fast'  # first run fast rewrites
        #     for _ in range(2):
        #         # this contains the context values inside of itself
        #         old = rexpr
        #         rexpr = self.apply(rexpr, top_disjunct_apply=True)
        #         if old == rexpr: break
        #         log_event('simplify_fast', old, rexpr)
        #         #import ipdb; ipdb.set_trace()
        #     #break
        #     self.__kind = 'full'  # run the full rewrites to make this
        #     old = rexpr
        #     rexpr = self.apply(rexpr, top_disjunct_apply=True)
        #     if old == rexpr: break
        #     log_event('simplify_full', old, rexpr)

        # if not add_context:
        #     return rexpr
        # return make_conjunction(self.context.local_to_rexpr(), rexpr)


    def rewrite_fast(self, rexpr, *, add_context=True):
        self.__kind = 'fast'
        for _ in range(5):
            old = rexpr
            rexpr = self.apply(rexpr, top_disjunct_apply=True)
            if old == rexpr: break
            log_event('simplify_fast', old, rexpr)
        if not add_context:
            return rexpr
        return make_conjunction(self.context.local_to_rexpr(), rexpr)

    def get_value(self, rexpr):
        # I think this method should get removed
        return self.context.get_value(rexpr)


    def should_defer_computing_memo(self, rexpr, arguments):
        # return true in the case there is not enough for this to attempt to memoize.  If this is too eager to try and compute a memo
        # then there will be _no_ advantage
        for a in arguments:
            if a is not None:  # meaning that there is /some/ value for this argument, though this might be less than we really want
                return False
        return True


# def fully_rewrite(rewrite_engine :RewriteEngine, rexpr):
#     while True:
#         old_rexpr = rexpr
#         rexpr = rewrite_engine(rexpr)
#         if old_rexpr == rexpr:  # meaning that there were no rewrites applied to the expression
#             break

def match(self :RewriteContext, rexpr, pattern, *pattern_args):
    # the pattern should be something which returns the variable
    # I suppose that we could use a for loop and then this would return an iterator in the case that the pattern matches

    # match might be outside
    if isinstance(pattern, str):
        expr = parse_sexp(pattern)
    else:
        assert isinstance(pattern, tuple)
        expr = pattern

    returning_match = []

    named_variables = {}

    def rec(rexpr, pattern):
        if isinstance(pattern, str):
            name = pattern
        else:
            name = pattern[0]
        if name == 'OR':
            for child in pattern[1:]:
                res = rec(rexpr, child)
                if res is not None:
                    return res
            return None  # failed to match
        elif name == 'AND':
            res = []
            for child in pattern[1:]:
                res = rec(rexpr, child)
                if res is None:
                    return None  # failed to match one of the expressions
            return res  # match was successful, return the last thing
        elif name == 'NOT':
            assert len(pattern) == 2
            res = rec(rexpr, pattern[1])
            if res is None:
                return []  # the match was unsuccessful, so this means we are negated in what is matched
            else:
                # there was some mat
                return None
        elif name == 'GET-VALUE':
            assert len(pattern) == 2
            # this is going to need to lookup the value of some variable from the envrionment
            res = rec(rexpr, pattern[1])
            if res is None or len(res) != 1:
                return None  # match failed or returned something unexpected
            if isVariable(res[0]):
                v = self.get_value(res[0])
                if v is None:
                    return None
                res = [v]
            return res
        elif name == 'let-var':
            assert len(pattern) == 3
            res = rec(rexpr, pattern[2])
            named_variables[pattern[1]] = res
            return res
        elif name == 'read-var':
            assert len(pattern) == 2
            return named_variables[pattern[1]]  # so that we can match something from before
        # elif name == 'match-var':
        #     assert len(pattern) == 2

        # TODO: maybe var, ground rexpr should be VAR GROUND REXPR so that it is clear they are meta
        elif name == 'var':
            if isVariable(rexpr):
                return [rexpr]  # this was a successful match, so return it in an array
            return None  # failed match
        elif name == 'ground':
            if isVariable(rexpr):
                if self is not None:
                    val = self.get_value(rexpr)
                    if val is not None:
                        return [val]
                return None  # not ground, so fail match
            elif contains_any_variable(rexpr):
                # then this would require looking up the variables to match a given expression
                # if there is something that does not match, then that means that this would

                # question: if the variables have known values, should those get unified in when returning, or should this just error out
                # and let those get unifie din elsewhere first.

                # we are not going to allow for this to match currently
                return None
            else:
                # this must be a ground value, so we can just return this
                return [rexpr]
        elif name == 'rexpr':
            return [rexpr]  # always matches
        elif name == 'param':
            assert len(pattern) == 2
            idx = int(pattern[1])
            if rexpr == pattern_args[idx]:
                return []  # successful match returns nothing
            else:
                return None  # match failed

        elif name == 'match-param':
            assert len(pattern) == 2
            idx = int(pattern[1])
            if rexpr == returning_match[idx]:
                return []  # successful matched something that was matched before
            else:
                return None
        elif name == 'read-param':
            assert len(pattern) == 2
            idx = int(pattern[1])
            return [pattern_args[idx]]

        elif name == 'EQ':
            assert len(pattern) == 3
            a = rec(rexpr, pattern[1])
            b = rec(rexpr, pattern[2])
            if a == b:
                return []
            else:
                return None
        elif name == 'NOT-EQ':
            assert len(pattern) == 3
            a = rec(rexpr, pattern[1])
            b = rec(rexpr, pattern[2])
            if a != b:
                return []
            else:
                return None
        elif name == 'mul':
            if pattern[1] == '>=':
                assert len(pattern) == 3
                # use like (mul >= 1)
                if isMultiplicity(rexpr) and rexpr.get_argument(0) >= int(pattern[2]):
                    return [rexpr.get_argument(0)]
                else:
                    return None
            assert len(pattern) == 2
            try:
                # use like (mul 0) to match
                val = int(pattern[1])
                if isMultiplicity(rexpr) and rexpr.get_argument(0) == val:
                    return []
                else:
                    return None
            except ValueError:
                if isMultiplicity(rexpr):
                    return []
                else:
                    return None

        elif name == 'any-disjunction':  # match any branch of the the disjunction
            assert len(pattern) == 2
            if rexpr.name == '+':
                for ags in rexpr.arguments:
                    res = rec(ags, pattern[1])
                    if res:
                        return res
                return None  # failed to match any of the disjunctions
            return rec(rexpr, pattern[1])  # this is not a disjunction, so just recurse on the expression

        # we are going to match against the name of the R-expr itself
        if rexpr.name != name:
            # the match has failed on the name alone
            assert '-' not in name  # otherwise something is a typo
            #print(f'attempting to match R-expr {name}')
            return None
        if isinstance(pattern, str):
            if rexpr.arity == 0:
                return []  # match a 0-arity term
            else:
                return None
        # there might be variable length matching here, so we are not going to check that the length matches

        ret = []
        term_idx = 0
        match_idx = 1
        while match_idx < len(pattern):
            # if term_idx >= rexpr.arity:
            #     return None  # match failed
            n = pattern[match_idx]
            if n == 'any':
                # then this is going to match from here to the end of the expression
                ret.append(rexpr.arguments[term_idx:])
                term_idx = rexpr.arity
                break
            elif n == 'args':
                num = int(pattern[match_idx + 1])
                match_idx += 1
                if rexpr.arity == num:
                    return []  # successful match
                else:
                    return None  # unsuccessful
            else:
                res = rec(rexpr.get_argument(term_idx), n)
                if res is None:
                    return None  # failed recursive match
                ret += res
            term_idx += 1
            match_idx += 1
        if term_idx != rexpr.arity:
            # then there are more terms which are not matched here, so this is a match failure
            return None
        return ret

    for e in expr:
        res = rec(rexpr, e)
        if res is None:
            return []  # meaning that the match has failed
        returning_match.extend(res)
    assert res is not None
    # this is to return an iterable over the values.  The returned iterable should always be length 1.
    # this match expression is either going to be used as `for val in match(....)` or `for a,b,c in match(...)`
    # which means that it needs to return the value as something that can be unpacked
    if len(returning_match) == 0:
        return [None] # this means the matches was successful, but we do not
                      # want to return an empty array as that means unsuccessful
    elif len(returning_match) == 1:
        # just return the array in this case as it will have the arguments
        # this should still be used as `for value in match(....)`
        return [returning_match[0]]
    else:
        # this has to return an iterable over the values, the
        return [returning_match]



rewrites = RewriteCollection()
register_rewrite = rewrites.register_function


####################################################################################################
# Rewrites


# @register_rewrite('(* any) (proj args 2) (aggregator ground var var rexpr)', kind='full')
# def dead_branch_elemination(self, rexpr):
#     return rexpr
#     all_branches = list(gather_branches(rexpr, through_nested=True))

#     if all_branches:
#         assert len(set(all_branches)) == len(all_branches) # this just makes the code easier to write on the next lines...
#         for branch in all_branches:
#             # we are going to check if there is some way to
#             for child in branch.arguments:
#                 nr = replace_term(rexpr, {branch: child})
#                 if is_empty(nr, 'fast'):
#                     import ipdb; ipdb.set_trace()

#         # print(rexpr)
#         # if rexpr.name == 'aggregator':
#         #     import ipdb; ipdb.set_trace()
#     return rexpr


@register_rewrite('(* any)')
def multipliy_base(self, rexpr):
    ret = []
    mul = 1
    for r in rexpr.arguments:
        z = self.apply(r)
        if isMultiplicity(z):  # this is going to want to match the multiplicity for some value
            mul *= z
            if mul == 0: return mul  # this has hit the shortcut of reducing to nothing
        else:
            ret.append(z)

    if mul != 1:
        ret.insert(0, multiplicity(mul))

    return make_conjunction(*ret)


def make_conjunction(*args):
    # helper function which flattened nested * expressions
    ret = []
    mul = 1
    def add(x):
        nonlocal mul, ret
        if isMultiplicity(x):
            mul *= x
        elif x.name == '*':
            for a in x.arguments:
                add(a)
        else:
            assert isinstance(x, Term)
            ret.append(x)
    for a in args:
        add(a)
        if mul == 0:
            # nothing else matters here
            return multiplicity(0)
    if mul != 1:
        ret.insert(0, mul)
    if len(ret) == 0:
        return multiplicity(1)
    if len(ret) == 1:
        return ret[0]
    return Term('*', ret)


@register_rewrite('(+ any)')
def add_base(self, rexpr):
    if rexpr.arity == 1:
        # then there is only 1 disjunction, so we should just remove the disjunction expression
        return self.apply(rexpr)

    ret = []
    env_outer= self.context
    try:
        for r in rexpr.arguments:
            # this will need to merge the common environments together
            # which means that this will need
            self.context = env_outer.copy(rexpr)
            try:
                r = self.apply(r, top_disjunct_apply=True)
                if not (isMultiplicity(r) and r == 0):  # ignore branches hwich are eleminated with 0 mult
                    ret.append((r, self.context))
            except UnificationFailure:
                assert False  # this should not get thrown (I think)
                # just ignore these branches
                pass

        if not ret:  # meaning that all ofthe branches are failed
            return multiplicity(0)

        # identify the common elements which are tracked in the environments by interesting the sets
        res_env = ret[0][1]
        for _, env in ret:
            res_env &= env

        for i in range(len(ret)):
            # now that the resulting environment has been identified, this can subtract
            # off that env and construct the resulting term for each sub expression
            re = ret[i][1] - res_env
            ret[i] = make_conjunction(ret[i][0], re.local_to_rexpr())

        # this has to update the context with everything instead of just overriding it
        env_outer.update(res_env)
        # env_outer = res_env
        # self.context = env_outer


        return make_disjunction(*ret)
    finally:
        # this will need to update the env_prev with whatever is the new content which
        self.context = env_outer
    # this will need to determine what is the common sets of these elements.  From there it will

def make_disjunction(*args):
    ret = []
    mul = 0
    def add(x):
        nonlocal mul, ret
        if isMultiplicity(x):
            mul += x
        elif x.name == '+':
            for a in x.arguments:
                add(a)
        else:
            assert isinstance(x, Term)
            ret.append(x)
    for a in args:
        add(a)
    if mul == float('inf'):
        # if this is `\infty + Q` then just return `\infty` as that is the rewrite
        return track_constructed(multiplicity(float('inf')), 'rr:infity_add',
                                 r'$\infty$\rterm{+Q} is rewritten as $\infty$.',
                                 args)
    if mul != 0:
        ret.insert(0, mul)
    if len(ret) == 0:
        return multiplicity(0)
    if len(ret) == 1:
        return ret[0]
    return Term('+', ret)


def make_unify(a,b):
    if not contains_any_variable(a) and not contains_any_variable(b):
        return multiplicity(1 if a == b else 0)
    return Term('=', (a,b))


@register_rewrite('(= args 2)', kind='full')
def unify_full(self, rexpr):
    for var, s in match(self, rexpr, '(= var rexpr)'):
        if not isVariable(s) and contains_any_variable(s) and self.context._parent is not None:
            #import ipdb; ipdb.set_trace()
            # then try to check if there are other unify expresions on this structure
            # and replace this structure with those variable names.  This could pass through /some/ distance
            ret = []
            for other in self.context._parent.get_associated_with_var(var):
                if other.name == '=' and other.arity == 2 and other is not rexpr and other.get_argument(0) == var:
                    # replace outselves with the other expression, so that this will find that this is
                    # this might want to only look into parent contexts so that we know that the other thing is at a higher level in the "stack"

                    ufw = other.get_argument(1)
                    if isinstance(ufw, Term) and not isVariable(ufw):
                        if ufw.name == s.name and ufw.arity == s.arity:
                            for a,b in zip(s.arguments, ufw.arguments):
                                ret.append(make_unify(a,b))
                        else:
                            ret.append(multiplicity(0))
            if ret:
                return make_conjunction(*ret)
                    # otherwise this is just some other variable, or a constant, or something, and we can not do the unification here

                    # import ipdb; ipdb.set_trace()
                    # pass

    return rexpr


@register_rewrite('(= args 2)')
def unify(self, rexpr):
    # if 'C' in str(rexpr.get_argument(0)) and 'hidden' in str(rexpr):
    #     print(rexpr)
    #     import ipdb; ipdb.set_trace()
    for a,b in match(self, rexpr, '(= ground ground)'):
        return multiplicity(1 if a == b else 0)
    for va, vb in match(self, rexpr, '(= var var)'):
        if va == vb:
            # this is trivally true, so just remove
            return track_constructed(multiplicity(1), 'rr:unify_same',
                                     r'This unification is trivially true, hence rewritten as \rterm{1}.',
                                     rexpr)

    for a,vb in match(self, rexpr, '(AND (NOT (= var rexpr)) (= rexpr var))'):
        # flip the direction
        rexpr = track_constructed(Term('=', (vb, a)), 'rr:unify_switch_order',
                                  r'Unification expression are normalized such that the variables always appear first.',
                                  rexpr)

    # rexpr here matches any term.  So if nither of these have a variable, then it will match this expression
    for va, vb in match(self, rexpr, '(AND (NOT (= var rexpr)) (NOT (= rexpr var)) (= rexpr rexpr))'):
        # then either this is a structured term, and we need to expand this out, or there  is some variable.
        # in this case, we do not want to track it as an assignment
        if not (isinstance(va, Term) and isinstance(vb, Term)):
            return track_constructed(multiplicity(0), 'rr:struct_unify1',
                                     r'Unification has failed due to mismatched functor names and values',
                                     rexpr)

        if va.name != vb.name or va.arity != vb.arity:
            # the arity on these expressions does not match
            return track_constructed(multiplicity(0), 'rr:struct_unify1',
                                     r'Unification has failed due to mismatched functor names: \rterm{'
                                     f'{va.name}/{va.arity}' r'} $\neq$ \rterm{' f'{vb.name}/{vb.arity}' r'}.',
                                     rexpr)

        # this is something like (f(x,y,z)=f(a,b,c)) so we want to expanded and construct a new term

        ret = [
            make_unify(aa,bb)
            for aa,bb in zip(va.arguments, vb.arguments)
        ]
        rr = track_constructed(make_conjunction(*ret), 'rr:struct_unify2',
                               r'This unification is expanded as the outer functor \rterm{' f'{va.name}/{va.arity}' r'} matches.',
                               rexpr)

        return self.apply(rr)

    # this is going to go into the environment, and then later pulled back out of the environment
    # so we don't want this to remain as it would end up duplicated
    self.context.add_rexpr(rexpr)
    if contains_any_variable(rexpr.get_argument(1)): # not isVariable(rexpr.get_argument(1)) and
        # this r-expr represents a unification with structure, which is not recreated by the Context by default
        # so we keep this r-expr around in the expression
        #import ipdb; ipdb.set_trace()

        # z = rexpr.get_argument(1)
        # if not (isVariable(z) or self.context.get_variable_type(rexpr.get_argument(0)) == (z.name, z.arity)):
        #     import ipdb; ipdb.set_trace()
        #     assert False

        return rexpr
    return multiplicity(1)


def make_project(*args):
    *var_names, rexpr = args
    for var in reversed(var_names):
        v = Variable(var)
        assert contains_variable(rexpr, v)
        rexpr = Term('proj', (v, rexpr))
    return rexpr

@register_rewrite('(proj var rexpr)')
def proj(self, rexpr):
    # if 'BB' in str(rexpr.get_argument(0)) and 'weight_output' in str(rexpr.get_argument(1)):
    #     import ipdb; ipdb.set_trace()
    # if 'AE' in str(rexpr.get_argument(0)):
    #     import ipdb; ipdb.set_trace()
    for v, r in match(self, rexpr, '(proj var rexpr)'):
        outer_context = self.context
        try:
            # if list(filter(lambda x: x.name != 'proj', outer_context.get_associated_with_var(v))):
            #     import ipdb; ipdb.set_trace()
            self.context = outer_context.copy(rexpr)
            #cinit = contains_variable(r, v)

            rr = self.apply(r, top_disjunct_apply=True)  # this is going to apply rewrites to the inner body
            vv = self.context.get_value(v)

            #assert vv is not None or contains_variable(rr, v)

            if vv is not None:
                # then we are going to go through and do a replace and then just return the body
                rr2 = replace_term(rr, {v: vv})  # replace the variable with its value
                rr2 = track_constructed(rr2, 'rr:equality_prop',
                                        f'The projected variable has been assigned and read from the context, its value is propagated through the \\rexpr.',
                                        Term('proj', (v, Term('*', (make_unify(v,vv), rr)))))
                outer_context.update_except(self.context, v)
                return rr2

            for _ in match(self, rr, '(mul 0)'):
                # if the body is zero, then this is also zero
                return track_constructed(multiplicity(0), ('rr:proj_no_var', 'rr:zero_mult'),
                                         r'The body of the projection is empty (\rterm{0})',
                                         rexpr)

            for _ in match(self, rr, '(any-disjunction (mul >= 1))'):
                # this is an expression like proj(X, 1+Q)  which rewrites as infinity as the variable X can take on any number of values
                # to match the paper this requires two rewrites
                assert vv is None # otherwise this somehow has some expression which resulted in the expression
                rr2 = track_constructed(multiplicity(float('inf')), ('rr:proj_no_var', 'rr:distribute-in-proj'),
                                         r'The expression \rterm{proj(X, 1)} is rewritten as $\infty$',
                                         rexpr)
                outer_context.update_except(self.context, v)
                return rr2

            outer_context.update_except(self.context, v)
            rr = make_conjunction(self.context.local_to_rexpr(), rr)
            #assert isMultiplicity(self.context.local_to_rexpr())

            # for gg in gather_environment(rr):
            #     if gg.name == '=' and gg.get_argument(0) == v:# and not contains_any_variable(gg.get_argument(1)):
            #         import ipdb; ipdb.set_trace()

            # remove disjunctive and conjunctive expressions out
            for ags in match(self, rr, '(+ any)'):
                ret = []
                for a in ags:
                    if vv is not None:
                        a = make_conjunction(make_unify(v, vv), a)
                    a = Term('proj', (v, a))
                    ret.append(a)
                res = make_disjunction(*ret)
                res = track_constructed(res, 'rr:distribute-in-proj',
                                        r'Disjunctive expressions like \rterm{proj(X, R+S)} are split into \rterm{proj(X, R)+proj(X, S)}',
                                        rexpr)
                return res

            for ags in match(self, rr, '(* any)'):
                not_depends = []
                depends = []
                if vv is not None:
                    depends.append(make_unify(v, vv))
                # anything which does not mention the variable v can be lifted out of the project statement
                for a in ags:
                    if contains_variable(a, v):
                        depends.append(a)
                    else:
                        not_depends.append(a)
                if not_depends:
                    outer_context.update_except(self.context, v)
                    # if 'AE' in str(not_depends):
                    #     import ipdb; ipdb.set_trace()
                    return track_constructed(
                        make_conjunction(*not_depends, Term('proj', (v, make_conjunction(*depends)))),
                        'rr:push-in-proj',
                        r'Sub expressions which do not depend on the projected variable are moved out, e.g. '
                        r'\rterm{proj(X, (Y=2)*R)} $\to$ \rterm{(Y=2)*proj(X, R)}.',
                        rexpr)


            for _ in match(self, rr, '(= (param 0) ground)', v):
                # this is proj(X, (X=5)) -> 1
                return track_constructed(multiplicity(1), 'rr:proj_occurs',
                                         r'This rewrites expressions like \rterm{proj(X, (X=5))} $\to$ \rterm{1}',
                                         rexpr)

            for _, _, nested_rexpr in match(self, rr, '(aggregator ground (param 0) var rexpr)', v):
                # this is proj(X, (X=sum(Y, ...)))
                if not contains_variable(nested_rexpr, v):
                    outer_context.update_except(self.context, v)
                    return track_constructed(multiplicity(1), 'rr:proj_nested_agg',
                                             r'This rewrites expressions like \rterm{proj(X, (X=sum(Y, R)))} $\to$ \rterm{1} as \rterm{(X=sum(Y, R)} will always \emph{eventually} rewrite as \rterm{(X=}\emph{some value}\rterm{)}',
                                             rexpr)

            if not contains_variable(rr, v):
                # this is "ok" in the case that the projection is going to eventually go to zero
                # otherwise this is going to have to take an inf multiplicity
                print('>>>>>>>>>>>>>>>>>>>>>>>>>>>> proj does not contain the variable that is being projected')


            return Term('proj', (v, rr))
        finally:
            # ccc2 = self.context
            self.context = outer_context
            # rrr = list(filter(lambda x: not (x.name == 'proj' and x.get_argument(0) == v), self.context.get_associated_with_var(v)))
            # ccc = self.context
            # if rrr:
            #     import ipdb; ipdb.set_trace()
            #     print('???')



@register_rewrite('(proj var rexpr)', kind='full')
def proj_full(self, rexpr):
    for v, r in match(self, rexpr, '(proj var rexpr)'):
        # this will want to walk through the expression to determine if there are any places where a variable is mentioned in a nested disjunction
        branches = list(gather_branches(r, through_nested=False))
        assert len(branches) == len(set(branches))  # make sure there are no duplicates (for now) as it makes the code easier

        # for every branch in the expression this will want to determine if the different disjunction reference the variable
        # from there it will want to split itself such that these branches are

        if branches:
            # this is a rewrite like `proj(V, R*(Q+S)) -> proj(V, R*Q)+proj(V,
            # R*S)` which could be a combination of the distribute rule with
            # expanding and then removing dijsunctionves from the expression.
            # Though we don't in general have this expand these rues out
            best_branch = None
            for b in branches:
                # if there is a branch which contains the variable that we are branching over, then favor that as it will be most helpful to us
                # this is just a heuristic, we could have selected any of the branches
                if contains_variable(b, v):
                    best_branch = b
                    break
            if best_branch is None: best_branch = branches[0]

            assert best_branch.name == '+'
            ret = []
            # this is going to want to match the disjuntion for a given expression
            for b in best_branch.arguments:
                nb = replace_term(r, {best_branch: b})
                ret.append(Term('proj', (v, nb)))

            # this rewrite should be a combination of a distributive rule and the above rule to split a disjunction.
            # though this requires
            return track_constructed(make_disjunction(*ret), ('rr:distributivity', 'rr:distribute-in-proj'),
                                     r'Nested distributive expressions under projections such as \rterm{proj(X, R*(Q+S))} are rewritten to \rterm{proj(X, R*Q)+proj(X, R*S)}.',
                                     rexpr)

        if not contains_variable(r, v):
            return track_constructed(make_disjunction(multiplicity(float('inf')), r),
                                     'rr:proj_no_var',
                                     'The projected variable does not appear in the expression, thus the projection can be removed',
                                     rexpr)

        # using = list(self.context.get_associated_with_var(v))
        # if len(using) == 2:
        #     # an expression like `proj(X, (X=f(Z))*R)` can just become `R` as `X` is dependent on `Z`
        #     import ipdb; ipdb.set_trace()

        # for gg in gather_environment(r):
        #     if gg.name == '=' and gg.get_argument(0) == v and not contains_any_variable(gg.get_argument(1)):
        #         import ipdb; ipdb.set_trace()


    return rexpr

agg_identity = {
    'sum': 0,
    'prod': 1,
    'min': float('inf'),
    'max': float('-inf'),
    'equals': Term('$null', ()),
}
agg_split_op = {
    'sum': 'plus',
    'prod': 'times',
    'min': 'min',
    'max': 'max',
    'equalsx': 'equals_agg_merge',
}
agg_run_op = {
    'sum': lambda a,b: a+b ,
    'prod': lambda a,b: a*b ,
    'min': min,
    'max': max,
    'equals': lambda a,b: b ,  # just return the last value
}

def make_aggregator(op, result, incoming, rexpr):
    # this needs to eleminate null expressions from these
    return make_conjunction(Term('is_not_null', (result,)), Term('aggregator', (op, result, incoming, rexpr)))


@register_rewrite('(aggregator ground var var rexpr)')
def aggregator(self, rexpr):
    # op, resulting, incoming, rexpr

    identity = agg_identity
    split_op = agg_split_op

    # aggregators where we have defined custom behavior for the expression
    special_aggregators = {
        'exists': 'exists',
    }

    if not isVariable(rexpr.get_argument(1)):
        return multiplicity(0)  # TODO: hack for now

    for op, resulting, incoming, rxp_orig in match(self, rexpr, '(aggregator ground var var rexpr)'):
        # this will need to match against the body of the expression
        outer_context = self.context
        try:
            has_not_null = Term('is_not_null', (resulting,)) in self.context.get_associated_with_var(resulting)
            self.context = outer_context.copy(rexpr)

            rxp = self.apply(rxp_orig, top_disjunct_apply=True)

            for _ in match(self, rxp, '(mul 0)'):
                #import ipdb; ipdb.set_trace()  # we might want to return dyna's $null here rather than the identity of the aggregator, though it might not matter for the examples that we are using this with
                #ident_val = identity[op]
                ident_val = null_val
                return track_constructed(make_unify(resulting, ident_val), 'rr:agg_sum1',
                                         r"The body of the aggregator is empty, hence the aggregator rewrites as the aggregator's identity: e.g. \rterm{(X=sum(Y, 0))}$\to$\rterm{(X=}\emph{identity}\rterm{)}",
                                         rexpr)
            for res in match(self, rxp, '(= (param 0) ground)', incoming):
                return track_constructed(make_unify(resulting, res), 'rr:agg_sum2',
                                         r'The aggregator knows its value as \rterm{(X=sum(Y, (Y=x)))}$\to$\rterm{(X=x)}.',
                                         rexpr)
            if ((inc_val := self.context.get_value(incoming)) is not None  # the variable is set
                and isMultiplicity(rxp)  # the body is trivial with begin only a multiplicty
                and len(self.context) == 1):
                # if the body is a multiplicity, then we should just return the resulting value
                # if len(self.context) > 1:
                #     # then this needs to read the other operations which are in the context,
                #     import ipdb; ipdb.set_trace()
                if rxp != 1:
                    v = inc_val
                    for _ in range(1, int(rxp)):
                        inc_val = agg_run_op[op](inc_val, v)
                    # import ipdb; ipdb.set_trace()
                    # assert rxp == 1  # TODO handle other multiplicies, will require knowing the sum_many operation
                return track_constructed(make_unify(resulting, inc_val), 'rr:agg_sum2',
                                         r"The aggregator knows its value as \rterm{(X=sum(Y, (Y=x)))}$\to$\rterm{(X=x)}, and the variable's value is looked up from the context \renv",
                                         rexpr)

            if has_not_null:
                outer_context.update_except(self.context, incoming)

            rxp = make_conjunction(self.context.local_to_rexpr(), rxp)

            for ags in match(self, rxp, '(+ any)'):
                # attempt to see if there are expressions which can be combined in the body using the aggregator
                accumulated_value = agg_identity[op]
                other = []
                for a in ags:
                    for val in match(self, a, '(= (param 0) ground)', incoming):
                        accumulated_value = agg_run_op[op](accumulated_value, val)
                        break  # this prevents the else branch from running if there was a match
                    else:
                        other.append(a)
                if accumulated_value != agg_identity[op]:  # if we did something
                    if other:
                        # then there are still things that need to run, so we are going to defer for now
                        other.append(make_unify(incoming, accumulated_value))
                        rxp = make_disjunction(*other)
                    else:
                        # there is only the result, so we can just return it directly
                        return track_constructed(make_unify(resulting, accumulated_value), ('rr:agg_sum2', 'rr:agg_sum3'),
                                                 r'The aggregator is rewritten as its final value', rexpr)

            for ags in []:#match(self, rxp, '(+ any)'):
                assert False
                # this is a disjunction between many different variables
                # this is going to have to construct many projects and new variables
                intermediate_vars = [generate_var() for _ in range(len(ags))]
                nested_exprs = []
                additional_exprs = []
                for nested_r, nv in zip(ags, intermediate_vars):
                    nested_exprs.append(Term('aggregator', (op, nv, incoming, nested_r)))
                    # track_constructed(Term('aggregator', (op, nv, incoming, nested_r)),
                    #                   'rr:agg_sum3',
                    #                   'Disjunctions in the aggregator are split up into nested \rterm{(X=sum(Y, R+S))}
                    #                   rexpr)
                while len(intermediate_vars) > 2:
                    # this is going to combine two of the variables together and generate a new variable to be the result
                    new_var = generate_var()
                    *intermediate_vars, v1, v2 = [new_var] + intermediate_vars
                    additional_exprs.append(Term(split_op[op], (v1, v2, new_var)))  # this is going to be like plus or times in the case

                if len(intermediate_vars) == 2:
                    additional_exprs.append(Term(split_op[op], (*intermediate_vars, resulting)))
                elif len(intermediate_vars) == 1:
                    # this should never happen as this should only happen in the case that there is some disjunction???
                    additional_exprs.append(make_unify(resulting, intermediate_vars[0]))
                else:
                    assert False  # should never happen

                # now this needs to construct the expression with all of the variables combined together
                nested_r = Term('*', nested_exprs + additional_exprs)
                for nv in intermediate_vars:
                    nested_r = Term('proj', (nv, nested_r))
                nested_r = track_constructed(nested_r, 'rr:agg_sum3',
                                             r'Disjunctions in the aggregators body are split, new intermediate variables are introdcued for '
                                             r'the results of these aggregators, and then the final value is set the the output varible.',
                                             rexpr)
                return make_conjunction(self.context.local_to_rexpr(), nested_r)

            for ags in match(self, rexpr, '(* any)'):
                not_depends = []
                depends = []
                for a in ags:
                    if contains_variable(a, incoming) or contains_variable(a, resulting):
                        depends.append(a)
                    else:
                        not_depends.append(a)
                if not_depends:
                    # then this could split the expression using sum copies rewrite rr:agg_split_conjunct
                    import ipdb; ipdb.set_trace()

            # no aggregator specific rewrites could be done, but there are likely rewrites which have been done on the inner part
            # so we are going to want to return that
            if rxp == rxp_orig and not self.context:
                # don't make something new as this would be the same expression (optimization)
                return rexpr

            #rxp = make_conjunction(self.context.local_to_rexpr(), rxp)  # anything from the environment that is set, track that here

            # lc = self.context
            # assert not lc
            return Term('aggregator', (op, resulting, incoming, rxp))
        finally:
            self.context = outer_context

    return rexpr
        # there are no rewrites which can be applied here
        #return rexpr


# @register_rewrite('(aggregator ground var var rexpr)', kind='full')
# def aggregator_full(self, rexpr):
#     return rexpr

#     split_op = agg_split_op
#     for op, result, incoming, rxp_orig in match(self, rexpr, '(aggregator ground var var rexpr)'):
#         branches = list(gather_branches(rxp_orig, through_nested=False))
#         assert len(branches) == len(set(branches))  # make sure there are no duplicates (for now) as it makes the code easier
#         if branches:
#             # TODO: this could break the expression part here using on of the nested branches
#             # this would be something like `A=sum(B, Q*(R+S)) -> ...sum(B, Q*R) + sum(B, Q*S)
#             branch = branches[0]
#             assert branch.name == '+'
#             intermediate_vars = [generate_var() for _ in range(branch.arity)]
#             nested_exprs = []
#             for iv, b in zip(intermediate_vars, branch.arguments):
#                 nested_exprs.append(Term('aggregator', (op, iv, incoming, replace_term(rxp_orig, {branch: b}))))
#             while len(intermediate_vars) > 2:
#                 # this is going to combine two of the variables together and generate a new variable to be the result
#                 new_var = generate_var()
#                 *intermediate_vars, v1, v2 = [new_var] + intermediate_vars
#                 nested_exprs.append(Term(split_op[op], (v1, v2, new_var)))  # this is going to be like plus or times in the case
#             assert len(intermediate_vars) == 2
#             nested_exprs.append(Term(split_op[op], (intermediate_vars[0], intermediate_vars[1], result)))
#             nested_r = make_conjunction(*nested_exprs)
#             for nv in intermediate_vars:
#                 nested_r = Term('proj', (nv, nested_r))



#             nested_r = track_constructed(nested_r, ('rr:agg_sum3', 'rr:distributivity'),
#                                          r'Nested disjunctions in an aggregator are split',
#                                          rexpr)
#             import ipdb; ipdb.set_trace()

#             return nested_r
#     return rexpr

@register_rewrite('(aggregator ground var var rexpr)', kind='full')
def aggregator_full(self, rexpr):
    for op, result, incoming, rxp_orig in match(self, rexpr, '(aggregator ground var var rexpr)'):
        has_not_null = Term('is_not_null', (result,)) in self.context.get_associated_with_var(result)
        if has_not_null:
            def lift_out(rx):
                if rx.name != '*':
                    return rx, multiplicity(1)
                depends, not_depends = [], []
                for a in rx.arguments:
                    if not contains_variable(a, incoming) and is_constraint(a):
                        not_depends.append(a)
                    else:
                        depends.append(a)
                if not not_depends:
                    return rx, multiplicity(1)
                return make_conjunction(*depends), make_conjunction(*not_depends)
            branches = list(gather_branches(rxp_orig, through_nested=False))
            if branches:
                # this finds expressions like `is_not_null(A)*(A=sum(B,
                # (X=123)*(B=3)+(X=22)*(B=3)))` and rewrites it as
                # `is_not_null(A)*((A=sum(B, (X=123)*(B=3)))+(A=sum(B,
                # (X=22)*(B=3))))` as we know that these two expressions will be
                # disjoint and when the result does not match it can be removed.  So there is no
                # need to leave them under the aggregator.  This will then
                assert branches[0].name == '+'
                original_elements = list(branches[0].arguments)
                branch_elements = [filter_out_variable(b, incoming) for b in branches[0].arguments]
                # if there is some base case multiplicity, then this means that
                # the expression is always added to the aggregator, and there is nothing
                contains_mult = False
                for b in branch_elements:
                    if isMultiplicity(b):
                        contains_mult = True
                if not contains_mult:
                    groupped_rexprs = []
                    if 'Loss' in str(result) and branches[0].arity > 15:
                        import ipdb; ipdb.set_trace()

                    while branch_elements:
                        e = branch_elements.pop(0)
                        grp = [original_elements.pop(0)]
                        i = 0
                        while i < len(branch_elements):
                            if not is_disjoint(e, branch_elements[i], kind='fast'):
                                # then this needs to add to the group as an overlapping element
                                branch_elements.pop(i)
                                grp.append(original_elements.pop(i))
                            else:
                                i += 1
                        groupped_rexprs.append(grp)
                    if len(groupped_rexprs) > 1:
                        # then we can split this aggregator up
                        res = []
                        for g in groupped_rexprs:
                            a,b = lift_out(make_disjunction(*g))
                            # if not isMultiplicity(b):
                            #     import ipdb; ipdb.set_trace()
                            res.append(make_conjunction(b, Term('aggregator', (op, result, incoming, a))))
                        return make_disjunction(*res)
            # # then we can instead try to lift out expressions which do not have to be included in the aggregator
            a,b = lift_out(rxp_orig)
            if not isMultiplicity(b):
                return make_conjunction(b, Term('aggregator', (op, result, incoming, a)))

        # all_branches = list(gather_branches(rxp_orig, through_nested=True))
        # # if len(all_branches) > 2:
        #     # this is doing dead (branch) code elemination for the expression
        # import ipdb; ipdb.set_trace()
    return rexpr




@register_rewrite('(aggregator_copies args 4)')
def aggregator_copies(self, rexpr):
    for op, result, incoming, n_copies_rexpr in match(self, rexpr, '(aggregator_copies ground var (OR ground var) rexpr)'):
        pass


# @register_rewrite('(exists var var rexpr)')
# def exists_aggregator(self, rexpr):
#     # if the result of the



@register_rewrite('(if args 3)')
def if_rr(self, rexpr):
    for cond, true_r, false_r in match(self, rexpr, '(if rexpr rexpr rexpr)'):
        # this needs to determine if the true branch matches something where there is non-zero multiplicity on one of the branches
        env_prev = self.context
        condition_res = None
        try:
            self.context = self.context.copy(rexpr)
            cond = self.apply(cond)
            cond = make_conjunction(self.context.local_to_rexpr(), cond)  # if there is an assignment, this should not get removed yet so unless it is already present in a higher context, then this needs to ignore this

            if isMultiplicity(cond):
                # check if the expression is like if(0,R,S) or if(1, R,S)
                if cond == 0: condition_res = False
                else: condition_res = True
            else:
                # check if the expression is like if(1+Q, R, S) in which case it is now true regardless of Q

                if match(self, cond, '(any-disjunction (mul >= 1))'):
                    condition_res = True
        finally:
            self.context = env_prev


        if condition_res is True:
            return self.apply(true_r)
        elif condition_res is False:
            return self.apply(false_r)
        else:
            # return unmodified as this was unable to determine if the condition is true or false
            # TODO: we could return the updated condition
            # it is "possible" to rewrite the true/false branches, but that means that it would not have the rewrites working correctly
            return rexpr

    return rexpr

def is_empty(r, kind='full'):
    with no_logging_rewrites():
        local_simplify = RewriteEngine(user_rewrites=False)  # do not do user rewrites here as that just slows this down
        if kind == 'full':
            r = local_simplify.rewrite_fully(r)
        else:
            r = local_simplify.rewrite_fast(r)
        if match(local_simplify, r, '(mul 0)'):
            return True
        return False


def is_disjoint(a,b, kind='full'):
    return is_empty(make_conjunction(a,b), kind)

@register_rewrite('(if args 3)', kind='full')
def if_rr_full(self, rexpr):

    for cond, true_r, cond2, true2_r, false_both in match(self, rexpr, '(if rexpr rexpr (if rexpr rexpr rexpr))'):

        # check if cond and cond2 are disjoint and if true_r and true2_r are disjoint
        if is_disjoint(cond, cond2) and is_disjoint(true_r, true2_r):
            return track_constructed(Term('if', (make_disjunction(cond, cond2), make_disjunction(true_r, true2_r), false_both)),
                                     'rr:merge_if',
                                     r'Two non-overlapping if expression can be merged into a single expression to make this more efficient',
                                     rexpr)

    return rexpr


##################################################

null_val = Term('$null', ())

@register_rewrite('(is_not_null args 1)')
def is_not_null(self, rexpr):
    for v in match(self, rexpr, '(is_not_null ground)'):
        return track_constructed(multiplicity(1 if v != null_val else 0),
                                 ('sec:trans'),
                                 r'Is not null checks its ground argument',
                                 rexpr)
    return rexpr

def cast_null(val, to):
    if val == null_val:
        return to
    return val

def is_null(*args):
    return all(v == null_val for v in args)

# somewhat conflated null casting rules so that it matches the aggregator, this should not

@register_rewrite('(plus args 3)')
def plus(self, rexpr):
    for a,b,c in match(self, rexpr, '(plus ground ground ground)'):
        # then this will match the ground values
        return multiplicity(1 if cast_null(a,0)+cast_null(b, 0) == cast_null(c, 0) else 0)
    for a,b, vc in match(self, rexpr, '(plus ground ground var)'):
        return Term('=', (vc, null_val if is_null(a,b) else cast_null(a,0) + cast_null(b, 0)))
    for a, vb, c in match(self, rexpr, '(plus ground var ground)'):
        return Term('=', (vb, null_val if is_null(c,a) else cast_null(c,0)-cast_null(a,0)))
    for va, b, c in match(self, rexpr, '(plus var ground ground)'):
        return Term('=', (va, null_val if is_null(c,b) else cast_null(c,0)-cast_null(b,0)))

    # return unchanged in the case that nothing matches
    return rexpr

@register_rewrite('(times args 3)')
def times(self, rexpr):
    cc = lambda x: cast_null(x, 1)
    for a,b,c in match(self, rexpr, '(times ground ground ground)'):
        return multiplicity(1 if cc(a)*cc(b) == cc(c) else 0)
    for a,b, vc in match(self, rexpr, '(times ground ground var)'):
        return Term('=', (vc, null_val if is_null(a,b) else cc(a)*cc(b)))
    for a, vb, c in match(self, rexpr, '(times ground var ground)'):
        return Term('=', (vb, null_val if is_null(a,c) else cc(c)/cc(a)))
    for va, b,c in match(self, rexpr, '(times var ground ground)'):
        return Term('=', (va, null_val if is_null(c,b) else cc(c)/cc(b)))
    return rexpr

@register_rewrite('(pow args 3)')
def power(self, rexpr):
    for a,b,c in match(self, rexpr, '(pow ground ground ground)'):
        return multiplicity(1 if a**b == c else 0)
    for a,b, vc in match(self, rexpr, '(pow ground ground var)'):
        return Term('=', (vc, a**b))
    for a,vb,c in match(self, rexpr, '(pow ground var ground)'):
        return Term('=', (vb, math.log(c)/math.log(a)))
    for va,b,c in match(self, rexpr, '(pow var ground ground)'):
        return Term('=', (va, c**(1/b)))
    return rexpr

@register_rewrite('(min args 3)')
def min_rr(self, rexpr):
    cc = lambda x: cast_null(x, float('inf'))
    for a,b,c in match(self, rexpr, '(min ground ground ground)'):
        return multiplicity(1 if min(cc(a),cc(b)) == cc(c) else 0)
    for a,b,vc in match(self, rexpr, '(min ground ground var)'):
        return Term('=', (vc, null_val if is_null(a,b) else min(cc(a),cc(b))))
    return rexpr

@register_rewrite('(max args 3)')
def max_rr(self, rexpr):
    cc = lambda x: cast_null(x, float('-inf'))
    for a,b,c in match(self, rexpr, '(min ground ground ground)'):
        return multiplicity(1 if max(cc(a),cc(b)) == cc(c) else 0)
    for a,b,vc in match(self, rexpr, '(max ground ground var)'):
        return Term('=', (vc, null_val if is_null(a,b) else max(cc(a),cc(b))))
    return rexpr

@register_rewrite('(equals_agg_merge args 3)')
def equals_agg_merge(self, rexpr):
    for a,b,c in match(self, rexpr, '(equals_agg_merge ground ground ground)'):
        if not ((a == c or is_null(a)) and (b == c or is_null(b))):
            return multiplicity(0)
        return multiplicity(1)
    for a,b,cv in match(self, rexpr, '(equals_agg_merge ground grond var)'):
        if is_null(a):
            return Term('=', (cv, b))
        elif is_null(b):
            return Term('=', (cv, a))
        else:
            assert a == b  # otherwise this means there are two different values for the equals expression, and this needs to return an "error"
            return Term('=', (cv, a)) # return null
    return rexpr

# technically these operations need to have an additional argument which is for
# it to always be returned true, as if the expression is `False is (a == b)`
# then it would be a not equal expressions

@register_rewrite('(lessthan args 2)')
def lessthan(self, rexpr):
    for a,b in match(self, rexpr, '(lessthan ground ground)'):
        return multiplicity(1 if a < b else 0)
    return rexpr


@register_rewrite('(lessthan args 2)', kind='full')
def lessthan_full(self, rexpr):
    for a,b in match(self, rexpr, '(lessthan var var)'):
        # this needs to read from the environment, though there might be multiple less than constraints that could work here
        pass

    # for a,b if match(self, rexpr, '(AND (lessthan (let-var a var) (let-var b ground)) (ENV (lessthan (EQ (read-var a) var) rexpr))'):
    #     # this needs to read from the environment

@register_rewrite('(lessthan_eq args 2)')
def lessthan_eq(self, rexpr):
    for a,b in match(self, rexpr, '(lessthan_eq ground ground)'):
        return multiplicity(1 if a <= b else 0)

    return rexpr

@register_rewrite('(equals args 2)')
def equals_rr(self, rexpr):
    for a,b in match(self, rexpr, '(equals ground ground)'):
        return multiplicity(1 if a == b else 0)
    return rexpr

# there is also range, not, not equal, abs etc which are not included


@register_rewrite('(bool_or args 3)')
def bool_or_rr(self, rexpr):
    for a,b,c in match(self, rexpr, '(bool_or ground ground ground)'):
        return multiplicity((a or b) == c)
    for a,b,vc in match(self, rexpr, '(bool_or ground ground var)'):
        return Term('=', (vc, (a or b)))
    # if either of the values is true, then we can match directly
    for a,b,vc in match(self, rexpr, '(AND (OR (bool_or (EQ ground (read-param 0)) rexpr rexpr) (bool_or rexpr (EQ ground (read-param 0)) rexpr)) (bool_or rexpr rexpr var))', True):
        return Term('=', (vc, True))

    return rexpr

@register_rewrite('(bool_and args 3)')
def bool_and_rr(self, rexpr):
    for a,b,c in match(self, rexpr, '(bool_and ground ground ground)'):
        return multiplicity((a and b) == c)
    for a,b,vc in match(self, rexpr, '(bool_and ground ground var)'):
        return Term('=', (vc, (a and b)))
    # if either of the values is false, then we can match directly
    for a,b,vc in match(self, rexpr, '(AND (OR (bool_and (EQ ground (read-param 0)) rexpr rexpr) (bool_and rexpr (EQ ground (read-param 0)) rexpr)) (bool_and rexpr rexpr var))', False):
        return Term('=', (vc, False))

    return rexpr

# define a rewrite which is just another R-expr rather than a "builtin"
rewrites.define_user_rewrite('bool', 1,
                             make_disjunction(
                                 Term('=', (Variable(0), True)),
                                 Term('=', (Variable(0), False))
                             ))


@register_rewrite('(exp args 2)')
def exp_rr(self, rexpr):
    # this is exponential rewrite
    for a,b in match(self, rexpr, '(exp ground ground)'):
        return multiplicity(1 if math.exp(a) == b else 0)
    for a,bv in match(self, rexpr, '(exp ground var)'):
        return Term('=', (bv, math.exp(a)))
    for av,b in match(self, rexpr, '(exp var ground)'):
        return Term('=', (av, 777))
        if b == 0:
            return Term('=', (av, float('-inf')))  # sigh
        return Term('=', (av, math.log(b)))
    return rexpr


def is_constraint(rexpr):
    # meaning that the arity of this expression is at _most_ 1
    return (rexpr.name, rexpr.arity) in (('=', 2), ('aggregator', 4), ('plus', 3),
                                         ('times', 3), ('pow', 3), ('min', 3), ('max', 3),
                                         ('equals_agg_merge', 3), ('lessthan', 2), ('bool_and', 3), ('bool_or', 3), ('is_not_null', 1))


####################################################################################################

@register_rewrite('(enforce_type args 3)')
def enforce_type(self, rexpr):
    return multiplicity(1)
    for a,b,c in match(self, rexpr, '(enforce_type ground ground ground)'):
        if isinstance(a, Term):
            typ = a.name, a.arity
            return multiplicity(1 if typ == (b, c) else 0)
        return multiplicity(0)
    for av,b,c in match(self, rexpr, '(enforce_type var ground ground)'):
        # this will force this to be regenerated in the local context where this is created
        typ = (b,c)
        ctyp = self.context.get_variable_type(av)
        if ctyp is None:
            self.context._variable_type_index[av] = typ
        else:
            assert typ == ctyp  # this is really just mult 0
        return multiplicity(1)



####################################################################################################

def example_fib_4_nomemo():
    simplify = RewriteEngine()
    rexpr = Term('fib', (4, Variable('res')))
    log_event('original_rexpr', rexpr)
    return simplify.rewrite_fully(rexpr)

def example_fib_4_memo():
    simplify = RewriteEngine()
    rewrites.set_memoized('fib', 2, 'unk')
    rexpr = Term('fib', (4, Variable('res')))
    log_event('original_rexpr', rexpr)
    return simplify.rewrite_fully(rexpr)


def example_peano():
    pass


def example_neural():
    simplify = RewriteEngine()
    # signmoid(X) = 1 / (1 + exp(-X))
    rewrites.define_user_rewrite(
        'sigmoid', 2,
        make_project('neg_X', 'exp_res', 'sum_res',
                     make_conjunction(
                         Term('times', (Variable(0), -1, Variable('neg_X'))),
                         Term('exp', (Variable('neg_X'), Variable('exp_res'))),
                         Term('plus', (1, Variable('exp_res'), Variable('sum_res'))),
                         Term('times', (Variable('sum_res'), Variable(1), 1))  # this is a division
                     )))
    rewrites.define_user_rewrite(
        'in', 2,
        make_aggregator('sum', Variable(1), Variable('agg_in'),
              make_project(
                'I', 'out_res', 'edge_res',
                  make_conjunction(
                      Term('edge', (Variable('I'), Variable(0), Variable('edge_res'))),
                      Term('out', (Variable('I'), Variable('out_res'))),
                      Term('times', (Variable('out_res'), Variable('edge_res'), Variable('agg_in')))
                  )
              )))
    rewrites.define_user_rewrite(
        'out', 2,
        make_aggregator('sum', Variable(1), Variable('agg_in'),
              make_disjunction(
                  make_project('in_res',
                               make_conjunction(
                                   Term('in', (Variable(0), Variable('in_res'))),
                                   Term('sigmoid', (Variable('in_res'), Variable('agg_in')))
                               )),
                  make_project('X', 'Y',
                               make_conjunction(
                                   Term('=', (Variable(0), Term('input', (Variable('X'), Variable('Y'))))),
                                   Term('pixel_brightness', (Variable('X'), Variable('Y'), Variable('agg_in')))
                               ))
              )))
    rewrites.define_user_rewrite(
        'loss', 1,
        make_aggregator('sum', Variable(0), Variable('agg_in'),
              make_project('J', 'out_res', 'target_res', 'diff_res',
                           make_conjunction(
                               Term('out', (Variable('J'), Variable('out_res'))),
                               Term('target', (Variable('J'), Variable('target_res'))),
                               Term('plus', (Variable('diff_res'), Variable('target_res'), Variable('out_res'))),  # the subtraction
                               Term('pow', (Variable('diff_res'), 2, Variable('agg_in')))
                           )
                        )))

    rewrites.define_user_rewrite(
        'edge', 3,
        make_aggregator('equals', Variable(2), Variable('agg_in'),
              make_disjunction(
                  make_project('X', 'Y', 'DX', 'DY', 'Xsum', 'Ysum',
                               make_conjunction(
                                   Term('=', (Variable(0), Term('input', (Variable('X'), Variable('Y'))))),
                                   Term('=', (Variable(1), Term('hidden', (Variable('Xsum'), Variable('Ysum'))))),
                                   Term('plus', (Variable('X'), Variable('DX'), Variable('Xsum'))),
                                   Term('plus', (Variable('Y'), Variable('DY'), Variable('Ysum'))),
                                   Term('weight_conv', (Variable('DX'), Variable('DY'), Variable('agg_in')))
                               )),
                  make_project('XX', 'YY', 'Property',
                               make_conjunction(
                                   Term('=', (Variable(0), Term('hidden', (Variable('XX'), Variable('YY'))))),
                                   Term('=', (Variable(1), Term('output', (Variable('Property'),)))),
                                   # Term('=', (Variable('XX'), 0)),
                                   # Term('=', (Variable('YY'), 0)),
                                   Term('weight_output', (Variable('Property'), Variable('XX'), Variable('YY'), Variable('agg_in'))),
                               ))
              )))

    weight_conv = {
        (0,0): 1,
        (-1,0): 2,
        (0,1): 3
    }

    rewrites.define_user_rewrite(
        'weight_conv', 3,
        make_disjunction(*[make_conjunction(
            Term('=', (Variable(0), x)),
            Term('=', (Variable(1), y)),
            Term('=', (Variable(2), val))
        ) for (x,y), val in weight_conv.items()]))

    weight_output = {
        ('cat', 0, 0): 2,
        ('dog', 0, 1): 1
    }

    rewrites.define_user_rewrite(
        'weight_output', 4,
        make_disjunction(*[make_conjunction(
            Term('=', (Variable(0), x)),
            Term('=', (Variable(1), a)),
            Term('=', (Variable(2), b)),
            Term('=', (Variable(3), val)),
        ) for (x, a,b), val in weight_output.items()]))


    # edge_values = {
    #     (0,0, 0): 123,
    #     (0,1, 1): 456,
    #     (1,0, 2): 789
    # }

    pixel_brightness = {
        (0,0): 1,
        (0,1): 2,
        (1,0): 3,
    }

    rewrites.define_user_rewrite(
        'pixel_brightness', 3,
        make_disjunction(*[make_conjunction(
            Term('=', (Variable(2), val)),
            Term('=', (Variable(0), x)),
            Term('=', (Variable(1), y))
        ) for (x,y), val in pixel_brightness.items()]))

    target_values = {
        Term('output', ('cat',)): 10,
        Term('output', ('dog',)): 5
    }

    rewrites.define_user_rewrite(
        'target', 2,
        make_disjunction(*[make_conjunction(
            Term('=', (Variable(0), x)),
            Term('=', (Variable(1), y))
        ) for x,y in target_values.items()]))

    # rewrites.define_user_rewrite(
    #     'edge', 3,
    #     make_disjunction(*[make_conjunction(
    #         Term('=', (Variable(0), Term('input', (x,y)))),
    #         Term('=', (Variable(1), z)),
    #         Term('=', (Variable(2), val))
    #     ) for (x,y,z), val in edge_values.items()]))

    rexpr = Term('loss', (Variable('loss_out'),))

    #rexpr = Term('in', (Variable(0), Variable(1)))
    #rexpr = Term('edge', (Variable('X'), Variable('Y'), Variable('Z')))
    #rexpr = Term('out', (Term('input', (Variable('x'), Variable('y'),)), Variable('z')))
    # rexpr = Term('out', (Term('hidden', (Variable('x'), Variable('y'))), Variable('z')))
    # rexpr = Term('in',  (Term('hidden', (Variable('x'), Variable('y'))), Variable('z')))
    # rexpr = Term('edge', (Variable('w'), Term('hidden', (Variable('x'), Variable('y'))), Variable('z')))
    # rexpr = Term('out', (Term('input', (Variable('x'), Variable('y'))), Variable('z')))
    # rexpr = make_conjunction(Term('out', (Variable('q'), Variable('z'))), Term('=', (Variable('q'), Term('input', (Variable('x'), Variable('y'))))))

    #rexpr = Term('edge', (Variable('w'), Term('input', (Variable('x'), Variable('y'))), Variable('z')))

    #rexpr = Term('edge', (Variable('X'), Term('hidden', (Variable('Q'), Variable('R'))), Variable('Z')))
    # rexpr = Term('edge', (Variable('Z'), Term('output', (Variable('X'), )),  Variable('W')))
    # rexpr = Term('edge', (Variable('Z'), Term('hidden', (Variable('X'), Variable('Y'))), Variable('W')))

    log_event('original_rexpr', rexpr)

    rr = simplify.rewrite_fully(rexpr)
    # simplify2 = RewriteEngine(user_rewrites=False)
    # rr2 = simplify2.rewrite_fully(rr)
    print('----------')
    print(rr)
    import ipdb; ipdb.set_trace()
    return rr, simplify.context



def main():
    global color_terminal, limit_printed_amount
    sys.setrecursionlimit(5000)
    #rexpr = Term('plus', (1,2,Variable('x')))

    #rexpr = Term('aggregator', ('sum', Variable('x'), Variable('y'), Term('+', (Term('=', (Variable('y'), 7)), Term('=', (Variable('y'), 10))))  ))  # (X=sum(Y, (Y=7)))

    # rexpr = Term('if', (Term('=', (Variable('x'), 3)), Term('plus', (1,Variable('x'), Variable('y'))), Term('plus', (4,Variable('x'), Variable('y')))))

    # rexpr = Term('*', (Term('=', (Variable('x'), 4)) , rexpr))

    #ctx = RewriteContext()
    simplify = RewriteEngine()

    if 0:
        rewrites.define_user_rewrite(
            'fib', 2,
            make_disjunction(
                make_conjunction(Term('=', (Variable(0), 0)), Term('=', (Variable(1), 0))),
                make_conjunction(Term('=', (Variable(0), 1)), Term('=', (Variable(1), 1))),
                make_project(
                    'sub1', 'sub2', 'res1', 'res2',
                    make_conjunction(
                        Term('lessthan', (1, Variable(0))),  # var_0 > 1
                        Term('plus', (Variable('sub1'), 1, Variable(0))),  # var_0 - 1
                        Term('plus', (Variable('sub2'), 2, Variable(0))),  # var_0 - 2
                        Term('fib', (Variable('sub1'), Variable('res1'))),  # res1 is fib(var_0 - 1)
                        Term('fib', (Variable('sub2'), Variable('res2'))),  # res2 is fib(var_0 - 2)
                        Term('plus', (Variable('res1'), Variable('res2'), Variable(1)))
                    ))
            )
        )
    else:
        rewrites.define_user_rewrite(
            'fib', 2,
            make_aggregator('sum', Variable(1), Variable('res'),
                  make_disjunction(
                      make_conjunction(Term('=', (Variable(0), 0)), Term('=', (Variable('res'), 0))),
                      make_conjunction(Term('=', (Variable(0), 1)), Term('=', (Variable('res'), 1))),
                      make_project(
                          'sub1', 'sub2', 'res1', 'res2',
                          make_conjunction(
                              Term('lessthan', (1, Variable(0))),  # var_0 > 1
                              Term('plus', (Variable('sub1'), 1, Variable(0))),  # var_0 - 1
                              Term('plus', (Variable('sub2'), 2, Variable(0))),  # var_0 - 2
                              Term('fib', (Variable('sub1'), Variable('res1'))),  # res1 is fib(var_0 - 1)
                              Term('fib', (Variable('sub2'), Variable('res2'))),  # res2 is fib(var_0 - 2)
                              Term('plus', (Variable('res1'), Variable('res2'), Variable('res')))
                          ))
                  ))
        )



    rewrites.define_user_rewrite(
        'peano', 2,
        make_disjunction(
            make_conjunction(Term('=', (Variable(0), Term('z', ()))), Term('=', (Variable(1), True))),
            make_project(
                'nested',
                make_conjunction(
                    Term('=', (Variable(0), Term('s', (Variable('nested'),)))),
                    Term('peano', (Variable('nested'), True)),
                    Term('=', (Variable(1), True)),  # this is the aggregator :- which means that there is going to be a true branch here
                )
            )
        )
    )

    def make_peano(v):
        r = Term('z', ())
        for _ in range(v): r = Term('s', (r,))
        return r

    generate_example = os.environ.get('GENERATE_EXAMPLE')
    if generate_example:
        generate_example = generate_example.replace('-', '_')
        globals()['example_'+generate_example]()
        generate_latex_file()
        return


    #example_fib_4_memo()
    #global color_terminal, limit_printed_amount
    color_terminal = True
    limit_printed_amount = False
    res, ctx = example_neural()
    generate_latex_file()


    print('-'*50)
    print(res)

    # import ipdb; ipdb.set_trace()

    return


    # #rexpr = Term('peano', (make_peano(5), Variable('res')))


    # #rewrites.set_memoized('fib', 2, 'unk')

    # rexpr = Term('fib', (4, Variable('res')))

    # #rexpr = Term('=', (Term('f', (1,2,3)), Term('f', (Variable('x'), Variable('y'), 3))))

    # log_event('original_rexpr', rexpr)

    # simplify.rewrite_fully(rexpr)


    # print('Original R-expr:', '-'*50)
    # print(rexpr.stylized_rexpr())

    # rexpr = simplify.rewrite_fully(rexpr)

    # print('-'*50)
    # print(rexpr)


    # for step in range(16):
    #     print('step:', step,'-'*50)
    #     rexpr = simplify.rewrite_once(rexpr)
    #     print(rexpr.stylized_rexpr())


    # print(rexpr.stylized_rexpr())
    # print('-'*50)

    # r = simplify.rewrite_once(rexpr)

    # print(r.stylized_rexpr())
    # print('-'*50)
    # r = simplify.rewrite_once(r)

    # print(r.stylized_rexpr())

if __name__ == '__main__':
    main()
