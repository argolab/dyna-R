import os
from functools import lru_cache

from arpeggio import (
    ParserPython, visit_parse_tree,
    ZeroOrMore, OneOrMore, EOF, Optional, 
)
from arpeggio import RegExMatch as _


binary_operators = [
    '*', '/', '+', '-'
]

def atom():
    return _('

def binary_op():
    return expression, binary_operators, expression

def expression():
    return 

def term():
    return '.'

def query():
    return '?'

def root():
    return [term, query]

parser = ParserPython(root, debug=True)


