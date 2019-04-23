class ModedOp(FBaseType):
    def __init__(self, name, ops, vars, nondet):
        self.vars_ = vars
        self.ops = ops
        self.name = name
        self.nondet = nondet
    @property
    def vars(self):
        return self.vars_
    def disp(self, indent):
        return f'{self.name}(' + ', '.join(map(str, self.vars)) + ')'
    def execute(self, frame):
        mode = tuple(frame.isBound(v) for v in self.vars)
        if mode in self.ops:
            vals = [frame.getVariable(v) for v in self.vars]
            return self.ops[mode](*vals)
        return ()
    def run(self, frame):
        r = self.execute(frame)
        if r is failure:
            return failedFrame, failure
        if r is error:
            return frame, error
        if r == ():
            return frame, self  # then we made no progress
        done_c = True
        for var, val in zip(self.vars, r):
            if hasattr(val, '__iter__'):
                if frame.isBound(var):
                    if frame.getVariable(var) not in val:
                        # we got an iterable that does not contain the currently assigned value
                        return failedFrame, failure
                else:
                    done_c = False
            elif val is None:
                done_c = False
            else:
                frame = frame.setVariable(var, val)
        return frame, ((failure if frame.isFailed() else done) if done_c else self)
    def rename_vars(self, remap):
        return ModedOp(self.name, self.ops, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def _get_iterators(self, ret):
        for var, val in zip(self.vars, self.execute(emptyFrame)):
            if hasattr(val, '__iter__'):
                ret[var].add(IteratorFromIterable(var, val))

def moded_op(name, op, nondet=False):
    arity = max(map(len, op.keys()))
    assert arity == min(map(len, op.keys()))
    def method_locations(*locs):
        assert len(locs) == arity
        return ModedOp(name, op, locs, nondet)
    method_locations.arity = arity
    return method_locations


class CheckOp(FBaseType):
    def __init__(self, name, op, vars):
        self.vars_ = vars
        self.name = name
        self.op = op
    @property
    def vars(self):
        return self.vars_
    def disp(self,indent):
        return f'{self.name}(' + ', '.join(map(str, self.vars)) + ')'
    def run(self, frame):
        if all(frame.isBound(v) for v in self.vars):
            vals = [frame.getVariable(v) for v in self.vars]
            if self.op(*vals):
                return frame, done
            else:
                return failedFrame, failure
        return frame, self
    def rename_vars(self, remap):
        return CheckOp(self.name, self.op, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op

# check only works in the fully ground case, so we don't care about any other modes atm
def check_op(name, op):
    arity = len(inspect.getfullargspec(op).args)
    def method_locations(*locs):
        if slower_checks:
            assert len(locs) == arity
        return CheckOp(name, op, locs)
    method_locations.arity = arity
    return method_locations


add = moded_op('add', {
    (True, True, True):  lambda a,b,c: (b+c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a-b) ,
    (True, False, True): lambda a,b,c: (a, a-c, c) ,
    (False, True, True): lambda a,b,c: (b+c, b, c) ,
})

sub = lambda a,b,c: add(b,a,c)

mul = moded_op('mul', {
    (True, True, True):  lambda a,b,c: (b*c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a/b) if b != 0 else error ,  # use the error state in div by 0
    (True, False, True): lambda a,b,c: (a, a/c, c) if c != 0 else error ,
    (False, True, True): lambda a,b,c: (b*c, b, c) ,
})

div = lambda a,b,c: mul(b,a,c)

range_v = moded_op('range', {
    (False, True, True): lambda a,b,c: (range(b,c), b, c) ,
    (True, True, True):  lambda a,b,c: (range(b,c), b, c) ,
})

abs_v = moded_op('abs', {
    (True,True):  lambda a,b: (abs(b), b) ,
    (False,True): lambda a,b: (abs(b), b) ,
    (True,False): lambda a,b: (a, [a,-a]) if a > 0 else ((a, 0) if a == 0 else error) ,
})

lt = check_op('lt', lambda a,b: a < b)

lteq = check_op('lteq', lambda a,b: a <= b)


# just rewrite in terms of lt so that we can demo the
# rewriting of range constraints into the range constraint
gt = lambda a,b: lt(b,a)
gteq = lambda a,b: lteq(b,a)


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
int_v = check_op('int', lambda x: isinstance(x, int))
float_v = check_op('float', lambda x: isinstance(x, float))
term_type = check_op('term_type', lambda x: False)
str_v = check_op('str', lambda x: isinstance(x, str))
bool_v = check_op('bool', lambda x: isinstance(x, bool))

import numpy as np
matrix_v = check_op('matrix', lambda x: isinstance(x, np.ndarray))

# would like to allow numpy style arrays as some primitive type.  Then we can figure out how to identify these cases
# and perform automatic rewrites? There should be some access operation, and then some einsum

class ArrayElement(FBaseType):
    # the keys are going to be desugared by the program.  So if the key was an
    # array which was positional.  Then we are only going to be taking the variables that should be ints that indicate
    # what value is contained in some slot.  Once the array is bound, we should be able to provide iterators over the domains
    # of the variables.
    def __init__(self, matrix, result, *keys):
        pass



class ArrayEinsum(FBaseType):
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



_builtins = {
    '+': add,
    '-': sub,
    '*', mul,
    '/', div,
    'range': range_v,
    'abs': abs_v,
    '<': lt,
    '<=', lteq,
    '>', gt,
    '>=': gteq,
    'int': int_v,
    'unify': unify
    'eq': unify,
}

builtins = {}
for k,v in _builtins.items():
    builtins[(k, v.arity)] = v
