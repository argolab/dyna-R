grammar = r"""
%import common.ESCAPED_STRING
%import common.WS
%import common.NEWLINE
%ignore WS
%ignore COMMENT
?start:  rules | query | db_literal //| dynadoc

///////////////////////////////////////////////////////////////////////////////

AGG.100: ( /[+*|&?:]=/ | /(max|min|bag|set)=/ | /:-/ | /->/ )

FLOAT: /-?[0-9]*((\.[0-9]+(e[\-+]?[0-9]+)?)|(e[\-+]?[0-9]+))/    // 1e5 and 1.0 and -1.0e+5

INT: /-?[0-9]+/

VAR: /[$]?[A-ZΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ][a-zA-Z0-9'_αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]*/

EQ: "="

LTE: "<="
GTE: ">="
GT: ">"
LT: "<"

FUNCTOR1: /([$]?[a-zαβγδεζηθικλμνξοπρστυφχψω]['a-zA-Z0-9_αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]*)/
FUNCTOR2: /('[^']+')/

COMMENT: "%" /[^\n\r]*/ (NEWLINE|/$/)

EOR: /\.\s+/
   | /\.\s*$/
   | /\.(?=[^\d])/

///////////////////////////////////////////////////////////////////////////////

?query: expr1 "?"

rules: ( rule )*

variable: VAR  -> variable
        | "_"  -> anon_var

rule: (term | variable) rhs EOR -> rule
    | term EOR -> rule_assertion   // special case for Prolog-style assertions.

///////////////////////////////////////////////////////////////////////////////

?p0: ( AGG | EQ ) expr1 -> term   // Can we just use the unary op mechanism?
   | term
   | literal
   | variable
   | "(" expr1 ")"

///////////////////////////////////////////////////////////////////////////////

rhs: ( AGG | EQ ) expr1

literal: ESCAPED_STRING -> string
       | INT            -> int
       | FLOAT          -> float
       | "true"         -> true
       | "false"        -> false
       | "$unk"         -> unk
       | "null"         -> null
       | list           -> noop

term: functor "(" expr2 [ "," expr2 ]* ")"
    | functor "(" ")"
    | functor

list: "[" expr3 [ "," expr3 ]* "|" expr3 "]" -> list_pattern_rest
    | "[" expr3 [ "," expr3 ]* "]"           -> list_pattern
    | "[" "]"                                -> list_pattern_empty

functor: FUNCTOR1 -> functor
       | "$" -> functor_tuple
       | FUNCTOR2 -> single_quote_functor

db_literal: "{" [ rule ]* "}"

//PS1: /^>/
//?cmd: PS1 /query/ expr1
//    | PS1 /.+/
//dynadoc: "<dynadoc>" [ rule | cmd ]* "</dynadoc>"

"""

R = 'R'; L = 'L'; N = 'N'

class BinOp:
    def __init__(self, symbol, assoc, priority, callback='binop'):
        assert assoc in (R, L, N)
        self.op = symbol
        self.assoc = assoc
        self.priority = priority
        self.callback = callback
    def render(self, OP, i):
        if self.assoc == L:
            l, r = i, i-1
        if self.assoc == R:
            l,r = i-1, i
        if self.assoc == N:
            l,r = i-1, i-1
        return f'?p{i}: p{l} {OP} p{r} -> {self.callback}  |  p{i-1} -> noop'


class UnaryOp:
    def __init__(self, symbol, priority, position='prefix', callback='unary_op'):
        self.op = symbol
        self.position = position
        self.priority = priority
        self.callback = callback
    def render(self, OP, i):
        # prefix
        if self.position == 'prefix':
            return f'?p{i}: {OP} p{i} -> {self.callback}  |  p{i-1} -> noop'
        else:
            assert self.position == 'postfix'
            return f'?p{i}: p{i} {OP} -> {self.callback}  |  p{i-1} -> noop'


class NullOp:
    def __init__(self, symbol, priority, callback='term'):
        self.op = symbol
        self.priority = priority
        self.callback = callback
    def render(self, OP, i):
        return f'?p{i}: {OP} -> {self.callback}  |  p{i-1} -> noop'


# TODO: implement if-then-else?
# TODO: Generalize to n-ary comparisons!
class ChainedCompare:
    def __init__(self, priority):
        self.op = None
        self.priority = priority
    def render(self, _, i):
        # [2019-03-10 Sun] Note: We needed to make chained comparisons and
        # ordinary comparisons one rule.
        return f"""\
?p{i}: p{i-1} ( (LT|LTE|GT|GTE) p{i-1} )*     -> chained_compare
"""

def add_infix_op():

    ops = [
        NullOp('*', 110),

        UnaryOp('+', 9),
        UnaryOp('-', 9),

        UnaryOp('&', 9),
        UnaryOp('*', 9),

        UnaryOp('!', 9),
        UnaryOp('?', 9),

        BinOp('^',  R, 8),    # power
        BinOp('/',  L, 7.5),  # floating-point division
        BinOp('//', L, 7.5),  # integer division
        BinOp('*',  L, 7),
        BinOp('+',  L, 6),
        BinOp('-',  L, 6.5),
        BinOp('is', N, 4),
        BinOp('↦', N, 4),

        BinOp('==', N, 4),
        BinOp('!=', N, 4),
        BinOp(':',  L, 4, 'colon'),     # left assoc: `X:a:b ==> ((X:a):b) ==> (X for X in a, X in b)`, after some simplification.
        BinOp('in', N, 4, 'contains'),
        BinOp('∈', N, 4, 'contains'),

        BinOp('<~', N, 4),

        BinOp('!',  L, 4, 'type_assertion'),   # left assoc just like :/2.

        ChainedCompare(5),

        BinOp('=',  N, 4),
        BinOp('=/=', N, 4),
        BinOp('&',  R, 3),
        BinOp('|',  R, 2),
        BinOp(',',  L, 1.1),

        BinOp('for', R, 1),    # `for/2` is `,/2` with the arguments reversed.
        BinOp('if', R, 1),
        BinOp('unless', R, 0),

        BinOp(';', L, 0),

        #BinOp('with_key', N, 4),
        BinOp('arg', N, 4)
    ]

    # operators at the same precedence level must have the

    Sym2Lex = {'=': 'EQ',
               '<=': 'LTE',
               '>=': 'GTE',
               '>': 'GT',
               '<': 'LT'}

    G = []
    expr1 = expr2 = expr3 = None
    for i, z in enumerate(sorted(ops, key=lambda x: -x.priority), start=1):

        # Figure out where these special positions are:
        #  - expr1: most general context
        #  - expr2: doesn't take the top-level comma as an operator (used in term and list)
        #  - expr3: list syntax: doesn't take the top-level comma and `|` has special meaning.
        if z.op == '|': expr3 = i-1
        if z.op == ',': expr2 = i-1
        expr1 = i

        if z.op is not None:
            if z.op in Sym2Lex:      # avoid lexer conflicts on '='
                OP = Sym2Lex[z.op]
            else:
                G.append(f'OP{i}: "{z.op}"')
                Sym2Lex[z.op] = OP = f'OP{i}'
        else:
            OP = None

        G.append(z.render(OP, i))

    G.extend([
        f'?expr1: p{expr1}',
        f'?expr2: p{expr2}',
        f'?expr3: p{expr3}',
    ])

    return '\n'.join(r.strip() for r in G)

grammar += add_infix_op()

# Cheatset: https://github.com/lark-parser/lark/blob/master/docs/lark_cheatsheet.pdf
from lark.lark import Lark
from lark.visitors import Transformer
from lark.exceptions import LarkError

class DynaParserException(Exception):
    pass

from dyna.syntax.generic import Term, FVar, Rule, fresh

# TODO: load math operators from a file with a bunch of pragmas which declare
# names (how to parse: operator precedence, associativity), types/modes ->
# FFI-calls (i.e., the built-in QPT), disposition,

class Cmd:
    def __init__(self, cmd):
        self.cmd = cmd
    def __repr__(self):
        return f'Cmd({self.cmd!r})'


class DynaTransformer(Transformer):

    def __init__(self):
        self.i = 0
        self.HANDLE_DISJUNCTIONS = True

    #__________________________________________________________________________
    #
    def dynadoc(self, x):
        return x

    def cmd(self, x):
        [y] = x
        return Cmd(str(y))

    #__________________________________________________________________________
    # List pattern-matching syntax.

    def list_pattern_rest(self, x):
        *xs, y = x
        for x in reversed(xs):
            y = Term('$cons', x, y)
        return y

    def list_pattern(self, xs):
        y = Term('$nil')
        for x in reversed(xs):
            y = Term('$cons', x, y)
        return y

    def list_pattern_empty(self, _):
        return Term('$nil')

    #__________________________________________________________________________
    # dynabase literals

    def db_literal(self, x):
        raise NotImplementedError()
        assert isinstance(x, list)
        from dyna.aggregators import AGG
        from dyna.answer import Result
        y = Result({}, default=None)
        for r in x:
            assert not r.sides
            y.contribute(r.head, AGG[r.aggr]([r.value]))
        return y

    #__________________________________________________________________________
    # binary operators

    # TODO: `colon` and `contains` don't need to be done by sugar.
    #
    #  - In general, the definition of `in/2` might want do different things
    #    depending on the "set" types, e.g., `X in [1,2,3]` should do something
    #    different than `X in int`.
    #
    #  - a better implementation would be able to splice `a` into `b` at
    #    runtime.  That would allow `b` to be a variable.
    #
    #  - It seems like in/2 suppress evaluation on the right?  So if we have
    #    `mylist=[1,2,3].` and we want to sum-up the elements we have to put an
    #    explicit evaluation `(+= Z in *mylist)`.
    #
    #   [2019-02-13 Wed] This issue with implementing `:/2` and `in/2` via
    #     builtins is that we seem to require normalization.
    #
    def colon(self, x):
        [a,_,_] = x

        # desugar -- eventually the right-hand side should be to type `set`. Are
        # all unary terms with true/null values instances of `sets`?
        # Technically every term (ground or nonground) may be coerced into a
        # set.  What would the `:` syntax mean for a term with arity!=1?

        #  (A:int) ==> ','(int(A), A)
        # f(X:int) ==>  f(','(int(X), X))   # leverages the fact that ',' returns the last value on success
        #
        # f(X:range(A,B)) ==>  f(','(range(X, A, B), X))

        return Term(',', self.contains(x), a)

    def type_assertion(self, x):
        [a,_,_] = x
        return Term(',', Term('$croak', self.contains(x), 'failed type assertion'), a)

    def contains(self, x):
        # desugar
        # (A in int) ==> ?int(A)
        [a,_,b] = x
        [fn,*args] = b

        # Special handling for `X in [1,2,3].`
        if ((fn == '$cons' and len(args) == 2) or (fn == '$nil' and len(args) == 0)):
            return Term('in_list', b, a)

        # the contains argument should be
        return Term(fn, *args, a)
        # return self.question_mark(Term(fn, a, *args))

    def question_mark(self, x):
        return Term('?', Term('&', x))   # suppress for now, but will be evaluated in the builin method
        #return Term('?', x)

    def chained_compare(self, x):
        if len(x) == 1:
            return x[0]
        else:
            # take adjacent pairs:
            #   0    1   2    3   4    5   6    7   8
            # [x0, op0, x1, op1, x2, op2, x3, op3, x4, ...]
            # (x0  op0  x1)     (x2  op2  x3)
            #          (x1  op1  x2)     (x3  op3  x4)
            conjunct = None
            for i in range(0, len(x)-2):
                if i % 2 == 1: continue
                a, op, b = x[i:i+3]
                y = Term(str(op.value), a, b)
                conjunct = Term('&', conjunct, y) if i > 0 else y
        return conjunct

    def binop(self, x):
        [a,op,b] = x
        if op == 'for':        # "for" desugars as "," with arguments swapped.
            return Term(',', b, a)
#        elif op == 'unless':
#            return Term(',', Term('!', b), a)   # `f(X) unless g(X)` ==> `!g(X), f(X)`

        elif op == '↦':           # same as `is/` with its arguments reversed.
            return Term('is', b, a)

        elif op == 'if':
            # TODO: if/2 is currently a synonym for `for/2`, but it should be a
            # more restricted version! In particular, it is semidet in a manner
            # than is enforced syntactically by disallowing free variables.
            # TODO: Implement if-then-else
            return Term(',', b, a)
        else:
            return Term(str(op), a, b)

    def unary_op(self, x):
        [op, a] = x
        if op == '?':
            return self.question_mark(a)
        return Term(str(op), a)

    def noop(self, x):
        [x] = x
        return x

    #__________________________________________________________________________
    # rule and rules

    def rule_assertion(self, x):
        [a, _] = x
        return Rule(a, ':-', True, [])

    def rule(self, x):
        [head, rhs, _] = x
        [agg, val] = rhs.children
        return Rule(head, agg.value, val, [])

    def rules(self, rs):
        # TODO: handling of prefix aggregators doesn't work on user_query
        # because we don't add rules to the dynabase in that case -- if we want
        # this mechanism to work there we have to allow for rules to be
        # temporary available during the necessary computation for the user
        # query -- then, I suppose we can drop it. Figuring out how to render
        # the query to the afterwords might be tricky because we applied a
        # nontrivial program transform.

        if self.HANDLE_DISJUNCTIONS:
            from dyna.syntax.prefixagg import handle_disjunctions
            return [rr for r in rs for rr in handle_disjunctions(r)]
        else:
            return rs

    #___________________________________________________________________________
    # variables, functors, terms

    def functor(self, x):
        [token] = x
        return str(token.value)

    def functor_tuple(self, _):
        return '$'

    def single_quote_functor(self, x):
        [token] = x
        return str(token.value)[1:-1]   # drop the single quotes

    def variable(self, x):
        [token] = x
        return FVar(token.value)

    def anon_var(self, _):
        self.i += 1
        return FVar(f'$Gen{self.i}')

    def term(self, x):
        [fn, *args] = x
        if fn == '$term':
            return Term(*args)
        return Term(fn,*args)

    #___________________________________________________________________________
    # Literals

    def null(self, _):
        return None

    def true(self, _):
        return True

    def false(self, _):
        return False

    def unk(self, _):
        from dyna.aggregators import UNK
        return UNK

    def string(self, x):
        [s] = x
        return s[1:-1].replace('\\"', '"')   # remove outer quotes and escaped quotes

    def float(self, x):
        [a] = x
        return float(a)

    def int(self, x):
        [a] = x
        return int(a)


transformer = DynaTransformer()
parser = Lark(grammar,
              parser = 'lalr',
              lexer = 'contextual',
              propagate_positions = True,
              transformer = transformer)


def run_parser(src, handle_disjunctions=True):
    "Returns the list of rules contained in src."
    from dyna.syntax.util import setunset
    with setunset(transformer, 'HANDLE_DISJUNCTIONS', handle_disjunctions):
        try:
            return parser.parse(src)
        except LarkError as e:
            raise DynaParserException('\n' + e.get_context(src))


def term(x):
    assert isinstance(x, str)
    return fresh(run_parser(f'{x}?'))


def test():
    src = """
    % Comments.
    b := f / 2.% for f(X).
    goal() += g(X) for X is f(-3*-6/7+-1e0^3).% inline g(X) += X.
%    goal(I<K) max=3 for h(X) is f(-X) * *&g(X,Y), a, b.
%    goal min= W.

    a :- f(c).

    a :- f(X: int).

    a :- f(X: range(3, 5)).


    simple.

    goal = a + b + c + d.

    h = tree(tree(a,b), tree(tree(1,2), d)).

    f([[a],[[X],Y|Xs]]).

    goal += f(X) * g(X, Y) for h(Y).

%    phrase(X,I,I+1) max= 1  with_key  X
%      for [I,X] in enumerate(sentence).

    enumerate([X|Xs], I) = [[I,X] | enumerate(Xs, I+1)].

    path(U,V) :- true for path(U,W), edge(W,V).
    length([]) += 0.
    length([X|Xs]) = 1+length(Xs).

    in_(X)."""

    #for x in parser.lex(src): print(f'lexeme: {x.type:15s} {x}')
    rules = run_parser(src)
    assert len(rules) == 15, len(rules)
    for r in rules:
        print(r)

    rules = run_parser("""
    % the prefix-aggregator will create two rules
    goal += f(X) * (+= g(X, Y) * h(Y)).
    """)
    assert len(rules) == 2


def more_tests():

    [r] = run_parser(r"""
    test("I'd like to \"quote\"") += 1.
    """)
    assert r.head.args[0] == 'I\'d like to "quote"'


    [r] = run_parser('b := f / 2.% for f(X).')
    assert r.head == Term('b')
    assert r.sides == []
    assert r.value == Term('/', Term('f'), 2)


    [r] = run_parser("""
    'escaped functor with "inner quotes" and symbols +_*&'(1,'part two', 5).
    """)

    print(r.head)
    assert r.head == Term('escaped functor with "inner quotes" and symbols +_*&', 1, Term('part two'), 5)
    assert r.aggr == ':-'
    assert r.value == True
    assert r.sides == []


    cons = lambda x, y: Term('$cons', x, y)
    nil = Term('$nil')
    [r] = run_parser("""f([ [a],
                            [ [X], 2 | Xs]
                          ]).""")
    assert r.head == Term('f', cons(cons(Term('a'), nil), cons(cons(cons(FVar('X'), nil), cons(2, FVar('Xs'))), nil)))


    #[r] = run_parser('path(U,V) :- true for path(U,W), edge(W,V).')
    #print(r)
    #print(r.circuit())

    #[r] = run_parser('path(U,V) :- path(U,W), edge(W,V).')
    #print(r)
    #print(r.circuit())


if __name__ == '__main__':
    test()
    more_tests()
