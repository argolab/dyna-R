# -*- coding: utf-8 -*-
"""
    pygments.lexers.prolog
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Prolog and Prolog-like languages.

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.style import Style
from pygments.lexer import RegexLexer, bygroups
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

from pygments.token import Keyword, Name, Comment, String, Error, \
    Number, Operator, Generic


class MyStyle(Style):
    default_style = ""
    styles = {
        Comment:                'italic #888',
        Keyword:                'bold #005',
        Name:                   '#f00',
        Name.Function:          '#0f0',
        Name.Class:             'bold #0f0',
        String:                 'bg:#eee #111',
        Keyword.Reserved:       'bold ',
    }



class DynaLexer(RegexLexer):
    """
    Lexer for Dyna files.
    """
    name = 'Prolog'
    aliases = ['prolog']
    filenames = ['*.dyna']
    mimetypes = ['text/x-dyna']

    flags = re.UNICODE | re.MULTILINE

    tokens = {
        'root': [
            (r'^#.*', Comment.Single),
            (r'/\*', Comment.Multiline, 'nested-comment'),
            (r'%.*', Comment.Single),

            # aggregator literal
            (r'(max|min|bag|set|[:+*&|?])=|:-|->', Keyword),

            # REPL commands
            ('(clear_memos|retract_rule|help|tickle|check_invariants|compare|obligated_children|rules|state|flush|agenda|query|retract_rule|state|memos|flush|agenda|run_agenda|subscribe|checkpoint|load|sol|verbose)',
             Keyword.Reserved),

            # FLOAT:
            ('-?[0-9]*((\.[0-9]+(e[\-+]?[0-9]+)?)|(e[\-+]?[0-9]+))', Number),
            # INT
            ('-?[0-9]+', Number),

            # VAR:
            ("[$]?[A-ZΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ][a-zA-Z0-9'_αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]*", Keyword),

            #EQ: "="

            # FUNCTOR1:
            ("([$]?[a-zαβγδεζηθικλμνξοπρστυφχψω]['a-zA-Z0-9_αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]*)", Name.Function),

            # FUNCTOR2:
            ("('[^']+')", Name.Function),

            #COMMENT: "%" /[^\n\r]*/ (NEWLINE|/$/)

            # EOR:
            ('\.\s+|\.\s*$|\.(?=[^\d])', Punctuation),
            ('[()]', Punctuation),

            # Strings
            (r'"((\\"|[^"])*?)"', String),

#            # character literal
#            (r'0\'.', String.Char),
#            (r'0b[01]+', Number.Bin),
#            (r'0o[0-7]+', Number.Oct),
#            (r'0x[0-9a-fA-F]+', Number.Hex),
#            # literal with prepended base
#            (r'\d\d?\'[a-zA-Z0-9]+', Number.Integer),
#            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
#            (r'\d+', Number.Integer),
#            (r'[\[\](){}|.,;!]', Punctuation),
#            (r':-|-->', Punctuation),
#            (r'"(?:\\x[0-9a-fA-F]+\\|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|'
#             r'\\[0-7]+\\|\\["\nabcefnrstv]|[^\\"])*"', String.Double),
#            (r"'(?:''|[^'])*'", String.Atom),  # quoted atom
#            # Needs to not be followed by an atom.
#            # (r'=(?=\s|[a-zA-Z\[])', Operator),
            (r'is\b', Operator),
            (r'(<|>|=<|>=|==|=:=|=|/|//|\*|\+|-)(?=\s|[a-zA-Z0-9\[])', Operator),
#            (r'(mod|div|not)\b', Operator),
            (r'_', Keyword),  # The don't-care variable
#            (r'([a-z]+)(:)', bygroups(Name.Namespace, Punctuation)),
#            (u'([a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
#             u'[\w$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*)'
#             u'(\\s*)(:-|-->)',
#             bygroups(Name.Function, Text, Operator)),  # function defn
#            (u'([a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
#             u'[\w$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*)'
#             u'(\\s*)(\\()',
#             bygroups(Name.Function, Text, Punctuation)),
#            (u'[a-z\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]'
#             u'[\w$\u00c0-\u1fff\u3040-\ud7ff\ue000-\uffef]*',
#             String.Atom),  # atom, characters
#            # This one includes !
#            (u'[#&*+\\-./:<=>?@\\\\^~\u00a1-\u00bf\u2010-\u303f]+',
#             String.Atom),  # atom, graphics
#            (r'[A-Z_]\w*', Name.Variable),
#            (u'\\s+|[\u2000-\u200f\ufff0-\ufffe\uffef]', Text),
        ],
        'nested-comment': [
            (r'\*/', Comment.Multiline, '#pop'),
            (r'/\*', Comment.Multiline, '#push'),
            (r'[^*/]+', Comment.Multiline),
            (r'[*/]', Comment.Multiline),
        ],
    }

    def analyse_text(text):
        return True
