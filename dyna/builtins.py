
from .interpreter import *

class ModedOp(RBaseType):
    def __init__(self, name, ops, vars, nondet):
        self.vars_ = vars
        self.ops = ops
        self.name = name
        self.nondet = nondet
    @property
    def vars(self):
        return self.vars_
    # def disp(self, indent):
    #     return f'{self.name}(' + ', '.join(map(str, self.vars)) + ')'
    # def execute(self, frame):
    #     mode = tuple(frame.isBound(v) for v in self.vars)
    #     if mode in self.ops:
    #         vals = [frame.getVariable(v) for v in self.vars]
    #         return self.ops[mode](*vals)
    #     return ()
    def rename_vars(self, remap):
        return ModedOp(self.name, self.ops, tuple(map(remap, self.vars)), self.nondet)
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def _get_iterators(self, ret):
        for var, val in zip(self.vars, self.execute(emptyFrame)):
            if hasattr(val, '__iter__'):
                ret[var].add(IteratorFromIterable(var, val))
    def _tuple_rep(self):
        return (self.__class__.__name__, self.name, self.vars)

@simplify.define(ModedOp)
def modedop_simplify(self, frame):
    mode = tuple(v.isBound(frame) for v in self.vars)
    if mode in self.ops:
        vals = tuple(v.getValue(frame) for v in self.vars)
        r = self.ops[mode](*vals)
        if isinstance(r, Terminal):
            return r
        if r == ():
            self  # made no progress
        done_c = all(not hasattr(v, '__iter__') for v in r)
        if done_c:
            # then we are going to set all of the resulting variables
            # then we can return that we are done
            for var, val in zip(self.vars, r):
                var.setValue(frame, val)
            return terminal(1)
    return self


def moded_op(name, op, nondet=False):
    arity = max(map(len, op.keys()))
    assert arity == min(map(len, op.keys()))
    return ModedOp(name, op, variables_named(ret_variable, *range(arity-1)), nondet)


class CheckOp(RBaseType):
    def __init__(self, name, op, vars):
        self.vars_ = vars
        self.name = name
        self.op = op
    @property
    def vars(self):
        return self.vars_
    def rename_vars(self, remap):
        return CheckOp(self.name, self.op, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def _tuple_rep(self):
        return (self.__class__.__name__, self.name, self.vars)


@simplify.define(CheckOp)
def checkop_simplify(self, frame):
    if all(v.isBound(frame) for v in self.vars[1:]):
        vals = [v.getValue(frame) for v in self.vars[1:]]
        res = self.op(*vals)
        assert isinstance(res, bool)
        self.vars[0].setValue(frame, res)
        return terminal(1)
    return self

# check only works in the fully ground case, so we don't care about any other modes atm
def check_op(name, arity, op):
    return CheckOp(name, op, variables_named(ret_variable, *range(arity)))

def true_return(expr):
    def f(ret, *args):
        assert False

        # the returned variable should just always be true, so if there are
        return intersect(unify(ret, constant(True)), expr(*args))
    return f


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


sub = lambda a,b,c: add(b,a,c)
dyna_system.define_term('sub', 2, sub)
dyna_system.define_term('-', 2, sub)

mul = moded_op('mul', {
    (True, True, True):  lambda a,b,c: (b*c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a/b) if b != 0 else error ,  # use the error state in div by 0
    (True, False, True): lambda a,b,c: (a, a/c, c) if c != 0 else error ,
    (False, True, True): lambda a,b,c: (b*c, b, c) ,
})
dyna_system.define_term('mul', 2, mul)
dyna_system.define_term('*', 2, mul)


div = lambda a,b,c: mul(b,a,c)
dyna_system.define_term('div', 2, div)
dyna_system.define_term('/', 2, div)



range_v = true_return(moded_op('range', {
    (False, True, True): lambda a,b,c: (range(b,c), b, c) ,
    (True, True, True):  lambda a,b,c: (range(b,c), b, c) ,
}))
#dyna_system.define_term('range', 3,

##################################################  TODO: define range, this needs the return value as well as the arguments?

abs_v = true_return(moded_op('abs', {
    (True,True):  lambda a,b: (abs(b), b) ,
    (False,True): lambda a,b: (abs(b), b) ,
    (True,False): lambda a,b: (a, [a,-a]) if a > 0 else ((a, 0) if a == 0 else error) ,
}, nondet=True))

lt = check_op('lt', 2, lambda a,b: a < b)

dyna_system.define_term('lt', 2, lt)
dyna_system.define_term('<', 2, lt)

lteq = check_op('lteq', 2, lambda a,b: a <= b)
dyna_system.define_term('lteq', 2, lteq)
dyna_system.define_term('<=', 2, lteq)



# just rewrite in terms of lt so that we can demo the
# rewriting of range constraints into the range constraint
gt = lambda a,b: lt(b,a)
gteq = lambda a,b: lteq(b,a)

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
    (False, False): lambda a,b: (True, [True, False]),
    (True, True): lambda a,b: (isinstance(b, bool), b),
    (True, False): lambda a,b:  (True, [True, False]),
}, nondet=True)


dyna_system.define_term('int', 1, int_v)
dyna_system.define_term('float', 1, float_v)
dyna_system.define_term('term_type', 1, term_type)
dyna_system.define_term('str', 1, str_v)
dyna_system.define_term('bool', 1, bool_v)



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



# _builtins = {
#     '+': add, 'add': add,
#     '-': sub, 'sub': sub,
#     '*': mul, 'mul': mul,
#     '/': div, 'div': div,
#     'range': range_v,
#     'abs': abs_v,
#     '<': lt, 'lt': lt,
#     '<=': lteq, 'lteq': lteq,
#     '>': gt, 'gt': gt,
#     '>=': gteq, 'gteq': gteq,
#     'int': int_v,
#     'unify': unify, 'eq': unify,
# }

# builtins = {}
# for k,v in _builtins.items():
#     builtins[(k, v.arity)] = v
