
from .interpreter import *

class ModedOp(RBaseType):
    def __init__(self, name, det, nondet, vars):
        self.det = det
        self.nondet = nondet
        self.name = name
        self.vars_ = vars
    @property
    def vars(self):
        return self.vars_
    def rename_vars(self, remap):
        return ModedOp(self.name, self.det, self.nondet, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def _tuple_rep(self):
        return (self.__class__.__name__, self.name, self.vars)

class IteratorFromIterable(Iterator):
    def __init__(self, variable, iterable):
        self.variable = variable
        self.iterable = iterable
    def bind_iterator(self, frame, variable, value):
        assert variable == self.variable
        if value in self.iterable:
            pass
        else:
            pass
    def run(self, frame):
        for v in self.iterable:
            yield {self.variable: v}
    @property
    def variables(self):
        return (self.variable,)


@simplify.define(ModedOp)
def modedop_simplify(self, frame):
    mode = tuple(v.isBound(frame) for v in self.vars)
    if mode in self.det:
        vals = tuple(v.getValue(frame) for v in self.vars)
        r = self.det[mode](*vals)
        if isinstance(r, FinalState):
            return r
        if r == ():
            return self  # made no progress
        for var, val in zip(self.vars, r):
            var.setValue(frame, val)
        return terminal(1)
    return self

@getPartitions.define(ModedOp)
def modedop_getPartitions(self, frame):
    mode = tuple(v.isBound(frame) for v in self.vars)
    if mode in self.nondet:
        # then this needs to get the iterator from the object and yield that
        # as a partition that can handle binding the particular variable

        vals = tuple(v.getValue(frame) for v in self.vars)
        r = self.nondet[mode](*vals)

        # these are cases which failed unification or something?  We need to
        # handle reporting errors in these cases as empty intersections
        assert r != () and not isinstance(r, FinalState)

        # TODO: this needs to handle all of the grounded variables first which
        # would have cases where we are checking if the value of a variable
        # unifies correctly

        for var, val in zip(self.vars, r):
            if hasattr(val, '__iter__'):
                # then this is a variable that we can iterate, so we want to do
                # that.  This should yield some iterator wrapper that is going
                # return the map to a variable.  This might also want to be able
                # to check contains, in which case, this should support the
                # overlapping behavior required for aggregation?

                yield IteratorFromIterable(var, val)
            elif not var.isBound(frame):
                yield SingleIterator(var, val)


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
    return moded_op(name, d)


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

mul = moded_op('mul', {
    (True, True, True):  lambda a,b,c: (b*c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a/b) if b != 0 else error ,  # use the error state in div by 0
    (True, False, True): lambda a,b,c: (a, a/c, c) if c != 0 else error ,
    (False, True, True): lambda a,b,c: (b*c, b, c) ,
})
dyna_system.define_term('mul', 2, mul)
dyna_system.define_term('*', 2, mul)


#div = lambda a,b,c: mul(b,a,c)
div = mul(ret_variable,1,ret=0)
dyna_system.define_term('div', 2, div)
dyna_system.define_term('/', 2, div)



# if this is allowed to unify with false, then this isn't quite right for the non-det version?
# that should really mark the first variable as being required as true, otherwise we are unable to unify?
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
dyna_system.define_term('abs', 2, abs_v)


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
term_type = check_op('term_type', 1, lambda x: False)
str_v = check_op('str', 1, lambda x: isinstance(x, str))
bool_v = moded_op('bool', {
    (False, True): lambda a,b: (isinstance(b, bool), b),
}, nondet={
    (True, False): lambda a,b:  (True, [True, False]),
})


dyna_system.define_term('int', 1, int_v)
dyna_system.define_term('float', 1, float_v)
dyna_system.define_term('term_type', 1, term_type)
dyna_system.define_term('str', 1, str_v)
dyna_system.define_term('bool', 1, bool_v)


def imath_op(name, op, inverse):
    d = {
        (True, True): lambda a,b: (op(b), b),
        (False, True): lambda a,b: (op(b), b),
        (True, False): lambda a,b: (a, inverse(a))
    }
    return moded_op(name, d)

import math
dyna_system.define_term('sin', 1, imath_op('sin', math.sin, math.asin))
dyna_system.define_term('cos', 1, imath_op('cos', math.cos, math.acos))
dyna_system.define_term('tan', 1, imath_op('tan', math.tan, math.atan))

dyna_system.define_term('sinh', 1, imath_op('sinh', math.sinh, math.asinh))
dyna_system.define_term('cosh', 1, imath_op('cosh', math.cosh, math.acosh))
dyna_system.define_term('tanh', 1, imath_op('tanh', math.tanh, math.atanh))

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
dyna_system.define_term('**', 2, pow_v)

# a = b // c
int_div = moded_op('int_div', {
    (False,True,True): lambda a,b,c: (b//c, b,c),
    (True,True,True): lambda a,b,c: (b//c, b,c),
    (True,True,False): lambda a,b,c: (a,b,b//a),
}, nondet={
    # TODO; double chekc this???
    (True,False,True): lambda a,b,c: (a,range(a*c, (a+1)*c), c) if a >= 0 else (a,range((a-1)*c, a*c), c)
})


import numpy as np
matrix_v = check_op('matrix', 1, lambda x: isinstance(x, np.ndarray))

# would like to allow numpy style arrays as some primitive type.  Then we can figure out how to identify these cases
# and perform automatic rewrites? There should be some access operation, and then some einsum

class ArrayElement(RBaseType):
    # the keys are going to be desugared by the program.  So if the key was an
    # array which was positional.  Then we are only going to be taking the variables that should be ints that indicate
    # what value is contained in some slot.  Once the array is bound, we should be able to provide iterators over the domains
    # of the variables.
    def __init__(self, matrix, result, *keys):
        pass



class ArrayEinsum(RBaseType):
    # operator can be a constant or some variable that will evaluate to a string.
    # The first variable will be the result, and then there are multiple arrays that are passed into numpy.einsum
    #
    # reverse mode not supported?  As that would require parsing the einsum and figuring out what
    #
    # This is created by identifying a rewrite such as a(I, J) += b(J) * C(I,J).
    # then this entire expression can be replaced with
    # intersect(Einsum('ij->j,ij', result_new_name, bref, cref), arrayElement(result_new_name, I, J))
    # this requires that b(J) and C(I,J) are already some array element accessors.  Which would require that was identified
    # based off how those elements were stored.  Or we might have that those are specified somehow already?


    def __init__(self, operator, result, *arrays):
        self.operator = operator
        self.result = result



# might also just have a lazy matrix type, like from mxnet or dynet.  then we
# can just identify cases where matrices are used, and there could be an element
# wise access operation.  Then we don't have to eventually upgrade from numpy


####################################################################################################
# infered constraints

if 0:
    # THIS ISN'T WORKING ATM, just brainstorming this
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


# this needs to be able ot check if some constraint could have been valid, so if
# 0 < C, if its value was known then it should be able to consider this
# constraint as having been included.  though might want to have some
# conditional branch way of including it



####################################################################################################
# bultins that are defined in terms of other R-exprs, these should probably just
# be defined in a prelude once there is some parser

from .terms import BuildStructure

# list_length(0, []).
# list_length(L+1, [X|Xs]) :- list_length(L, Xs).
list_length = intersect(Unify(constant(True), ret_variable),  # set the "returned" variable as just always true, might not have this with :-???
                        partition(variables_named(0,1),
                                  (intersect(Unify(constant(0), VariableId(0)), BuildStructure('nil', VariableId(1), ())),
                                   intersect(add(constant(1), VariableId('len1'), ret=VariableId(0)),
                                             BuildStructure('.', VariableId(1), (VariableId('X'), VariableId('Xs'))),
                                             gteq(VariableId(0), constant(1)),
                                             # this is the recursive call
                                             # TODO: this should not have to reference the system? so there should be some placeholder here instead....
                                             dyna_system.call_term('list_length', 2)(VariableId('len1'), VariableId('Xs'), ret=VariableId('ret_ignore'))
                                   ))))

dyna_system.define_term('list_length', 2, list_length)
