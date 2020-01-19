from .interpreter import *


def infer_modes(d):
    # determine other modes that we can support
    # so if something supports the free mode, then it will also support the ground mode,
    # this will automatically infer those cases
    done = False
    while not done:
        done = True
        for k in list(d.keys()):
            for i, v in enumerate(k):
                if v is False:  # meaning it supports the free mode
                    nk = k[:i] + (True,) + k[i+1:]
                    if nk not in d:
                        done = False
                        d[nk] = d[k]
    return d

def moded_op(name, det, *, nondet={}):
    o = det.copy()
    o.update(nondet)
    arity = max(map(len, o.keys()))
    assert arity == min(map(len, o.keys()))

    det = infer_modes(det)
    nondet = infer_modes(nondet)
    for k in list(nondet.keys()):
        if k in det:
            del nondet[k]  # we don't want to run a non-det operation in the case that we have a det handler

    return ModedOp(name, det, nondet, variables_named(ret_variable, *range(arity-1)))


# check only works in the fully ground case, so we don't care about any other modes atm
def check_op(name, arity, op):
    f = lambda x, *args: (op(*args), *args)
    d = {
        (False,)+((True,)*arity): f
    }
    r = moded_op(name, d)

    # if arity == 1:  # if there is only one argument, then we want to force the result to be true to match the behavior of :-
    #     r = intersect(Unify(constant(True), ret_variable), r(0))

    return r



##################################################

from .context import dyna_system


add = moded_op('add', {
    (True, True, True):  lambda a,b,c: (b+c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a-b) ,
    (True, False, True): lambda a,b,c: (a, a-c, c) ,
    (False, True, True): lambda a,b,c: (b+c, b, c) ,
})
dyna_system.define_term('add', 2, add)  # there is the result variable that is always named ret, so this is still +/2
dyna_system.define_term('+', 2, add)


#sub = lambda a,b,c: add(b,a,c)
sub = add(ret_variable,1,ret=0)
dyna_system.define_term('sub', 2, sub)  # The pattern matching is happing on the ModedOp, so this should still pattern match with the add op
dyna_system.define_term('-', 2, sub)

# unary_minus = moded_op('unary_-', {
#     (True, True): lambda a,b: (-b, b),
#     (False, True): lambda a,b: (-b, b),
#     (True, False): lambda a,b: (a, -a),
# })
dyna_system.define_term('-', 1, sub(constant(0), 0, ret=ret_variable))

mul = moded_op('mul', {
    (True, True, True):  lambda a,b,c: (b*c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a/b) if b != 0 else error ,  # use the error state in div by 0
    (True, False, True): lambda a,b,c: (a, a/c, c) if c != 0 else error ,
    (False, True, True): lambda a,b,c: (b*c, b, c) ,
})
dyna_system.define_term('mul', 2, mul)
dyna_system.define_term('*', 2, mul)


div = mul(ret_variable,1,ret=0)
dyna_system.define_term('div', 2, div)
dyna_system.define_term('/', 2, div)


# defined for tim's parser
dyna_system.define_term(
    ',', 2,
    intersect(Unify(VariableId(0), constant(True)),
              Unify(VariableId(1), ret_variable))
)



# if this is allowed to unify with false, then this isn't quite right for the non-det version?
# that should really mark the first variable as being required as true, otherwise we are unable to unify?
# TODO: the order of arguments needs to be changed to match the colon operator
range_v = moded_op('range', {
    (False, True, True, True, True):  lambda x,a,b,c,d: (a in range(b,c,d), a, b, c, d) ,
}, nondet={
    (False, False, True, True, True): lambda x,a,b,c,d: (True, range(b,c,d), b, c,d) ,
})
dyna_system.define_term('range', 3, range_v(0,1,2,constant(1),ret=ret_variable))  # with a step of 1
dyna_system.define_term('range', 4, range_v)


##################################################  TODO: define range, this needs the return value as well as the arguments?

abs_v = moded_op('abs', {
    (False,True): lambda a,b: (abs(b), b) ,
}, nondet={
    (True,False): lambda a,b: (a, [a,-a]) if a > 0 else ((a, 0) if a == 0 else error) ,
})
dyna_system.define_term('abs', 1, abs_v)


lt = check_op('lt', 2, lambda a,b: a < b)

dyna_system.define_term('lt', 2, lt)
dyna_system.define_term('<', 2, lt)

lteq = check_op('lteq', 2, lambda a,b: a <= b)
dyna_system.define_term('lteq', 2, lteq)
dyna_system.define_term('<=', 2, lteq)



# just rewrite in terms of lt so that we can demo the
# rewriting of range constraints into the range constraint
# gt = lambda a,b: lt(b,a)
# gteq = lambda a,b: lteq(b,a)

gt = lt(1,0,ret=ret_variable)  # just flip the arguments order
gteq = lteq(1,0,ret=ret_variable)

dyna_system.define_term('gt', 2, gt)
dyna_system.define_term('>', 2, gt)
dyna_system.define_term('gteq', 2, gteq)
dyna_system.define_term('>=', 2, gteq)



unary_not = moded_op('not', {
    (False, True): lambda a,b: (not b, bool(b)),
    (True, False): lambda a,b: (bool(a), not a)
})

dyna_system.define_term('!', 1, unary_not)

binary_eq = check_op('==', 2, lambda a,b: a == b)
dyna_system.define_term('==', 2, binary_eq)

binary_neq = check_op('!=', 2, lambda a,b: a != b)
dyna_system.define_term('!=', 2, binary_neq)

dyna_system.define_term(
    '=', 2,
    intersect(
        Unify(ret_variable, constant(True)),
        Unify(VariableId(0), VariableId(1)))
)


class AndOperator(RBaseType):
    def __init__(self, ret, a, b):
        super().__init__()
        self.ret = ret
        self.a = a
        self.b = b
    @property
    def vars(self):
        return self.ret, self.a, self.b
    def rename_vars(self, remap):
        a = remap(self.a)
        b = remap(self.b)
        if a == b:
            return unify(remap(self.ret), a)
        return AndOperator(remap(self.ret), a,b)

@simplify.define(AndOperator)
def simplify_andoperator(self, frame):
    ret = self.ret.getValue(frame)
    a = self.a.getValue(frame)
    b = self.b.getValue(frame)
    if ret is True:
        self.a.setValue(frame, True)
        self.b.setValue(frame, True)
        return terminal(1)
    if a is True and b is True:
        self.ret.setValue(frame, True)
        return terminal(1)
    if (a is not InvalidValue and not a) or (b is not InvalidValue and not b):
        self.ret.setValue(frame, False)
        return terminal(1)
    if a is True:
        return Unify(self.ret, self.b)
    if b is True:
        return Unify(self.ret, self.a)
    return self

dyna_system.define_term('&', 2, AndOperator(ret_variable, VariableId(0), VariableId(1)))
dyna_system.define_term('&&', 2, AndOperator(ret_variable, VariableId(0), VariableId(1)))


class OrOperator(RBaseType):
    def __init__(self, ret, a, b):
        super().__init__()
        self.ret = ret
        self.a = a
        self.b = b
    @property
    def vars(self):
        return self.ret, self.a, self.b
    def rename_vars(self, remap):
        a = remap(self.a)
        b = remap(self.b)
        if a == b:
            return unify(remap(self.ret), a)
        return OrOperator(remap(self.ret), a,b)

@simplify.define(OrOperator)
def simplify_oroperator(self, frame):
    ret = self.ret.getValue(frame)
    a = self.a.getValue(frame)
    b = self.b.getValue(frame)
    if ret is False:
        self.a.setValue(frame, False)
        self.b.setValue(frame, False)
        return terminal(1)
    if a is False and b is False:
        self.ret.setValue(frame, False)
        return terminal(1)
    if (a is not InvalidValue and a) or (b is not InvalidValue and b):
        self.ret.setValue(frame, True)
        return terminal(1)
    if a is False:
        return Unify(self.ret, self.b)
    if b is False:
        return Unify(self.ret, self.a)
    return self

dyna_system.define_term('|', 2, OrOperator(ret_variable, VariableId(0), VariableId(1)))
dyna_system.define_term('||', 2, OrOperator(ret_variable, VariableId(0), VariableId(1)))


import random
random_seed = random.randint(0, 2**63)

def random_value(element, low, high):
    e = hash(element) ^ random_seed
    r = random.Random()
    r.seed(e)
    return r.uniform(low, high)

random_r = moded_op('random', {
    (False, True, True, True): lambda a,b,c,d: (random_value(b,c,d), b,c,d)
})

dyna_system.define_term('random', 3, random_r)
dyna_system.define_term('random', 1, random_r(VariableId(0), constant(0.0), constant(1.0), ret=ret_variable))



# we should really have some special denotation for types then we can add some
# rewrites that check that types are consistent and eleminate branches?  I
# suppose that only needs to happen for the case that we are dealing with
# primitive types.  And then more complicated types can just inherit where
# approperate?
#
# does there need to be some casting of types as well?  Those would actually
# need to modify a variable to match a particular type
#
# maybe we can just use the rewrite that looks at conjunctive intersecting
# constraints and pull constraints down.  It would just rewrite pairs of
# constraints as failed, so int(X), str(X) => Terminal(0)?  Then we can use the
# same identification of the same constraints
# to eleminate duplicated checks in the program
#
# the quotes are handled specially to deal with the names and their arities.  in
# the case of nested expressions, we are going to be able to handle rewrites of
# unions of variables across different branches.
int_v = check_op('int', 1, lambda x: isinstance(x, int))
float_v = check_op('float', 1, lambda x: isinstance(x, float))
from .terms import Term
term_type = check_op('term_type', 1, lambda x: isinstance(x, Term))
str_v = check_op('str', 1, lambda x: isinstance(x, str))
bool_v = moded_op('bool', {
    (False, True): lambda a,b: (isinstance(b, bool), b),
}, nondet={
    # this isn't quite right, as what if someone wrote `False is bool(X)`, then we should not iterate the domain of a boolean variable
    (True, False): lambda a,b:  (True, [True, False]),
})


# just a partition over the two different identifies for these expressions
number_v = partition((VariableId(0), ret_variable), (int_v, float_v))
primitive_v = partition((VariableId(0), ret_variable), (int_v, float_v, str_v, bool_v))

dyna_system.define_term('int', 1, int_v)
dyna_system.define_term('float', 1, float_v)
dyna_system.define_term('term_type', 1, term_type)
dyna_system.define_term('str', 1, str_v)
dyna_system.define_term('bool', 1, bool_v)
dyna_system.define_term('number', 1, number_v)
dyna_system.define_term('primitive', 1, primitive_v)  # anything that is not a term


def imath_op(name, op, inverse):
    d = {
        (True, True): lambda a,b: (op(b), b),
        (False, True): lambda a,b: (op(b), b),
        (True, False): lambda a,b: (a, inverse(a))
    }
    return moded_op(name, d)

def def_inverse(op, inv):
    r = dyna_system.call_term(op, 1)
    dyna_system.define_term(inv, 1, r(ret_variable,ret=0))

import math
dyna_system.define_term('sin', 1, imath_op('sin', math.sin, math.asin))
dyna_system.define_term('cos', 1, imath_op('cos', math.cos, math.acos))
dyna_system.define_term('tan', 1, imath_op('tan', math.tan, math.atan))

def_inverse('sin', 'asin')
def_inverse('cos', 'acos')
def_inverse('tan', 'atan')

dyna_system.define_term('sinh', 1, imath_op('sinh', math.sinh, math.asinh))
dyna_system.define_term('cosh', 1, imath_op('cosh', math.cosh, math.acosh))
dyna_system.define_term('tanh', 1, imath_op('tanh', math.tanh, math.atanh))

def_inverse('sinh', 'asinh')
def_inverse('cosh', 'acosh')
def_inverse('tanh', 'atanh')

exp = imath_op('exp', math.exp, math.log)
dyna_system.define_term('exp', 1, exp)
dyna_system.define_term('log', 1, exp(ret_variable,ret=0))

pow_v = moded_op('pow', {
    (True,True,True):  lambda a,b,c: (b**c,b,c),
    (False,True,True): lambda a,b,c: (b**c,b,c),
    (True,True,False): lambda a,b,c: (a,b,math.log(a,b)),
    (True,False,True): lambda a,b,c: (a,a**(1/c),c)
})
dyna_system.define_term('pow', 2, pow_v)
dyna_system.define_term('^', 2, pow_v)

# a = b // c
int_div = moded_op('int_div', {
    (False,True,True): lambda a,b,c: (b//c, b,c),
    (True,True,True): lambda a,b,c: (b//c, b,c),
    (True,True,False): lambda a,b,c: (a,b,b//a),
}, nondet={
    # TODO; double chekc this???
    (True,False,True): lambda a,b,c: (a,range(a*c, (a+1)*c), c) if a >= 0 else (a,range((a-1)*c, a*c), c)
})

mod_v = moded_op('mod', {
    (False,True,True): lambda a,b,c: (b%c,b,c)
})
dyna_system.define_term('mod', 2, mod_v)

import numpy as np
matrix_v = check_op('matrix', 1, lambda x: isinstance(x, np.ndarray))

# would like to allow numpy style arrays as some primitive type.  Then we can figure out how to identify these cases
# and perform automatic rewrites? There should be some access operation, and then some einsum

# class ArrayElement(RBaseType):
#     # the keys are going to be desugared by the program.  So if the key was an
#     # array which was positional.  Then we are only going to be taking the variables that should be ints that indicate
#     # what value is contained in some slot.  Once the array is bound, we should be able to provide iterators over the domains
#     # of the variables.
#     def __init__(self, matrix, result, *keys):
#         super().__init__()
#         pass



# class ArrayEinsum(RBaseType):
#     # operator can be a constant or some variable that will evaluate to a string.
#     # The first variable will be the result, and then there are multiple arrays that are passed into numpy.einsum
#     #
#     # reverse mode not supported?  As that would require parsing the einsum and figuring out what
#     #
#     # This is created by identifying a rewrite such as a(I, J) += b(J) * C(I,J).
#     # then this entire expression can be replaced with
#     # intersect(Einsum('ij->j,ij', result_new_name, bref, cref), arrayElement(result_new_name, I, J))
#     # this requires that b(J) and C(I,J) are already some array element accessors.  Which would require that was identified
#     # based off how those elements were stored.  Or we might have that those are specified somehow already?


#     def __init__(self, operator, result, *arrays):
#         super().__init__()
#         self.operator = operator
#         self.result = result



# might also just have a lazy matrix type, like from mxnet or dynet.  then we
# can just identify cases where matrices are used, and there could be an element
# wise access operation.  Then we don't have to eventually upgrade from numpy


####################################################################################################
# infered constraints


dyna_system.define_infered(
    intersect(int_v('v'), gteq('v', 'a'), lt('v', 'b')),
    range_v('v', 'a', 'b'))

dyna_system.define_infered(
    intersect(int_v('v'), gt('v', 'a'), lt('v', 'b')),
    # then we need to add a new variable that is 1 greater than a for the range constraint as it normally includes the lower bound
    intersect(add('a', constant(1), ret='_ap1'), range_v('v', '_ap1', 'b'))  # the `_` would indicate that this needs to allocate a new variable in this case
)

dyna_system.define_infered(
    # a < b < c => a < c
    intersect(lt('a', 'b'), lt('b', 'c')),
    lt('a', 'c')
)

dyna_system.define_infered(
    intersect(lt('a', 'b'), lt('b', 'a')),
    Terminal(0)  # failure, there is no value such that a < b & b < a
)

dyna_system.define_infered(
    intersect(lteq('a', 'b'), lteq('b', 'a')),
    Unify('a', 'b')  # a <= b & b <= a  ==>  a == b
)

dyna_system.define_infered(
    lt('a', 'b'),
    lteq('a', 'b')  # would like this to be able to use this to identify redudant constraints also, which means that we can delete stuff?
)

dyna_system.define_infered(
    # in the case that the same variable is used for multiplication, then would like to identify it as a power
    # as teh power operator supports the mode where 'a' is not grounded
    mul('a', 'a', ret='b'),  # a*a == a^2
    pow_v('a', constant(2), ret='b')
)

dyna_system.define_infered(
    intersect(pow_v('a', 'b', ret='c'), mul('a', 'c', ret='d')),  # a^b * a == a^(b + 1)
    intersect(pow_v('a', '_s', ret='d'), add('b', constant(1), ret='_s'))
)

dyna_system.define_infered(
    intersect(pow_v('a', 'b', ret='c'), pow_v('c', 'd', ret='e')),  # (a^b)^d == a^(b*d)
    intersect(pow_v('a', '_m', ret='e'), mul('b', 'd', ret='_m'))
)


# this needs to be able ot check if some constraint could have been valid, so if
# 0 < C, if its value was known then it should be able to consider this
# constraint as having been included.  though might want to have some
# conditional branch way of including it


# if one of the variables is bound as a constant, then we should be allowed to
# just evaulate the constraint directly to see if it is true.

dtypes = [int_v, float_v, term_type, str_v, bool_v]

for i, a in enumerate(dtypes):
    for b in dtypes[i+1:]:
        # for types that do not overlap, we are going to mark these as terminal
        # up front, so we can just delete these branches
        if not (a in (int_v, float_v) and b in (int_v, float_v)):
            dyna_system.define_infered(
                intersect(a('a'), b('a')),
                Terminal(0)
            )



####################################################################################################
# bultins that are defined in terms of other R-exprs, these should probably just
# be defined in a prelude once there is some parser

from .terms import BuildStructure, Term

# list_length(0, []).
# list_length(L+1, [X|Xs]) :- list_length(L, Xs), L >= 0.
# list_length = intersect(Unify(constant(True), ret_variable),  # set the "returned" variable as just always true, might not have this with :-???
#                         partition(variables_named(0,1),
#                                   (intersect(Unify(constant(0), VariableId(0)), BuildStructure('nil', VariableId(1), ())),
#                                    intersect(add(constant(1), VariableId('len1'), ret=VariableId(0)),
#                                              BuildStructure('.', VariableId(1), (VariableId('X'), VariableId('Xs'))),
#                                              gteq(VariableId(0), constant(1)),
#                                              # this is the recursive call
#                                              # TODO: this should not have to reference the system? so there should be some placeholder here instead....
#                                              dyna_system.call_term('list_length', 2)(VariableId('len1'), VariableId('Xs'), ret=constant(True))
#                                    ))))

# dyna_system.define_term('list_length', 2, list_length)


# prepend([], A, A).
# prepend([X|Y], A, [X|B]) :- prepend(Y, A, B).
# append = intersect(Unify(constant(True), ret_variable),
#                     partition(variables_named(0,1,2),
#                               (intersect(Unify(constant(Term('nil', ())), VariableId(0)), Unify(VariableId(1), VariableId(2))),
#                                intersect(BuildStructure('.', VariableId(0), [VariableId('X'), VariableId('Y')]),
#                                          BuildStructure('.', VariableId(2), [VariableId('X'), VariableId('B')]),
#                                          dyna_system.call_term('append', 3)(VariableId('Y'), VariableId(1), VariableId('B')))
#                               )))
# dyna_system.define_term('append', 3, append)


# this isn't going to quite make sense.  These more realistically would perform unification between these expressions
# though then that makes it so that unification can be overwritten such that terms which are not equal are unifable together...
# if this was just used by the aggregators, then maybe that would be ok?  It would allow for aggregators to combine a value

# `a` should already be a term, as it would have to indirected through the term object to get to this point already
dyna_system.define_term('__builtin_term_compare_<', 2, check_op('__builtin_term_compare_<', 2, lambda a, b: a.builtin_lt(b)))
#dyna_system.define_term('__builtin_term_compare_==', 2, check_op('__builtin_term_compare_==', 2, lambda a, b: a.builtin_eq(b)))



# additional methods required to make tim's parser list processing work
dyna_system.define_term('$cons', 2, BuildStructure('.', ret_variable, (VariableId(0), VariableId(1))))
dyna_system.define_term('$nil', 0, BuildStructure('nil', ret_variable, ()))


dyna_system.define_term('$null', 0, BuildStructure('$null', ret_variable, ()))


from .terms import Evaluate, ReflectStructure

# $reflect(Out, Name :str, arity :int, [arg1, arg2, arg3...])
# arity allows for this to be rewritten eariler, but is optional as it can be infered if the list is fully ground
dyna_system.define_term('$reflect', 4, Intersect((Unify(constant(True), ret_variable), ReflectStructure(VariableId(0), VariableId(1), VariableId(2), VariableId(3)))))
# $reflect(Out, Name :str, [arg1, arg2, arg3...])
dyna_system.define_term('$reflect', 3, Intersect((Unify(constant(True), ret_variable), ReflectStructure(VariableId(0), VariableId(1), VariableId('not_used'), VariableId(2)))))

# Z is $call(&foo(1,2,3), X) => Z is foo(1,2,3,X)
# the parser should just use Evaluate directly from terms, as this is only going up to 8 (which matches the prolog docs...)
for i in range(8):
    dyna_system.define_term('$call', i+1, Evaluate(dyna_system, ret_variable, VariableId(0), tuple(VariableId(j+1) for j in range(i))))
