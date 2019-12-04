# builtin operations which are for representing expressions as matrices and using external matrix calls when

import numpy as np

from .interpreter import *
from .builtins import moded_op, check_op, mul as builtin_multiply
from .context import dyna_system
from .optimize import optimizer

USE_PYTORCH = False

if not USE_PYTORCH:
    # numpy
    class DynaMatrix(object):
        """
        A wrapper for numpy matrices to use with Dyna
        """

        def __init__(self, matrix):
            self.matrix = matrix
            self.shape = matrix.shape
            self.lazy = False  # if this needs to be evaluated?

        def __getitem__(self, key):
            r = self.matrix[key]
            if np.isscalar(r):
                return r  # just return the value directly
            return DynaMatrix(r)

        def __matmul__(self, other):
            # ideally this would allow for lazy operations to be performed.  Would
            # like general eigensum representation between matrices.  Would allow
            # for this to be more efficiently represented
            return DynaMatrix(self.matrix @ other.matrix)

else:
    import torch

    class DynaMatrix(object):
        """
        A wrapper for PyTorch tenors for use with Dyna
        """

        def __init__(self, matrix):
            self.matrix = matrix
            self.shap = matrix.shape

        def __getitem__(self, key):
            r = self.matrix[key]
            if np.isscalar(r):
                return r  # just return the value directly
            return DynaMatrix(r)

        def __matmul__(self, other):
            # ideally this would allow for lazy operations to be performed.  Would
            # like general eigensum representation between matrices.  Would allow
            # for this to be more efficiently represented
            return DynaMatrix(self.matrix @ other.matrix)



class NDArrayAccess(RBaseType):
    "Access an element of an NDArray"
    def __init__(self, args :Tuple[Variable], ret :Variable, array_ref :Variable):
        super().__init__()
        self.args = args
        self.ret = ret
        # a variable that references the matrix object.
        # can be a constant in the case that the matrix is already specified.  Though might also
        # be a variable that is filled in by another expression
        self.array_ref = array_ref
    @property
    def vars(self):
        return *self.args, self.ret, self.array_ref
    def rename_vars(self, remap):
        return NDArrayAccess(tuple(remap(v) for v in self.args), remap(self,ret), remap(self.array_ref))

@simplify.define(NDArrayAccess)
def NDArrayAccess_simplify(self, frame):
    if not self.array_ref.isBound(frame):
        return self  # the array is not set, there is nothing that we are able to do in this case
    array = self.array_ref.getValue(frame)
    if not isinstance(array, DynaMatrix):
        assert False  # something has gone wrong.  The array ref variable should never be anything other than
        return terminal(0)
    if all(v.isBound(frame) for v in self.args):
        # just lookup the value and set the result
        try:
            val = array[tuple(v.getValue(frame) for v in self.args)]
            self.ret.setValue(frame, val)
            return terminal(1)
        except IndexError:
            # then this is outside of the bounds of this array
            return terminal(0)
    elif any(v.isBound(frame) for v in self.args):
        # if there are any variables which are bound then we can simplify this
        # expression by taking a slice
        key = tuple(v.getValue(frame) if v.isBound(frame) else slice(None) for v in self.args)
        try:
            r = array[key]
            return NDArrayAccess(tuple(v for v in self.args if not v.isBound(frame)), self.ret, r)
        except IndexError:
            return terminal(0)
    else:
        # TODO: in the case that ret is bound, then this might be able to search the matrix
        # otherwise this is going to end up performing a scan with just a loop in python....
        return self

@getPartitions.define(NDArrayAccess)
def NDArrayAccess_partition(self, frame):
    if self.array_ref.isBound(frame):
        array = self.array_ref.getValue(frame)
        for i, var in enumerate(self.args):
            if not var.isBound(frame):
                yield IteratorFromIterable(var, range(array.shape[i]))


class MakeNDArrayAccess(RBaseType):
    """
    Take an existing R-expr, and attempt to convert it into a matrix form and then return
    """

    def __init__(self, args :Tuple[Variable], ret :Variable, array_ref :Variable, wrapped :RBaseType):
        super().__init__()
        self.args = args
        self.ret = ret
        self.array_ref = array_ref
        self.wrapped = wrapped

    @property
    def vars(self):
        return *self.args, self.ret, self.array_ref
    @property
    def children(self):
        return (self.wrapped,)
    def rename_vars(self, remap):
        return MakeNDArrayAccess(tuple(remap(a) for a in self.args), remap(self.ret), remap(self.array_ref), self.wrapped.rename_vars(remap))
    def rewrite(self, rewriter):
        return MakeNDArrayAccess(self.args, self.ret, self.array_ref, rewriter(self.wrapped))

@simplify.define(MakeNDArrayAccess)
def MakeNDArrayAccess_simplify(self, frame):
    body = simplify(self.wrapped, frame)
    args = tuple(a for a in self.args if not a.isBound(frame))
    if not args or isinstance(body, FinalState):
        return body
    body = partition((*args, self.ret), [body])
    body = simplify(b, frame, reduce_to_single=False, flatten_keys=True)

    max_key = [0]*len(args)
    for key, c in body._children:
        assert c == Terminal(1)
        # otherwise we can not convert this to a matrix
        # this means that we can not perform the more effiicent matrix operation
        # I suppose that in this case, it could just fail and return the result of the partition directly?
        # though that would mean that something else would have to handle the "unoptimize-able" result
        for i, k in enumerate(key[::-1]):
            assert isinstance(k, int)
            if k >= max_key[i]:
                max_key[i] = k+1
        assert np.isscalar(key[-1])

    array = np.zeros(max_key)
    for key, c in body._children:
        array[key[::-1]] = key[-1]

    return NDArrayAccess(args, self.ret, constant(array))


@optimizer.define(NDArrayAccess)
def NDArrayAccess_optimize(self, info):

    # identify if there are two different matrix constraints that could be merged
    # this is basically that there is some shared parameter and that there is
    # is some multiplication between the results of two expressions
    # and that there is a multiplication between the different inputs

    valid = True

    processed_variables = set()
    nd_arrays = set()
    aggregator = None

    # this needs to identify if the output variable is feeding into an aggregtor
    # which is +=, and if all of the operations are multiplications


    next_var = set(self.ret)
    while next_var:
        looking_var = next_var.pop()
        if looking_var in processed_variables:
            continue
        processed_variables.add(processed_variables)
        agg_constraints = info.conjunctive_constraints[looking_var]
        for R in agg_constraints:
            if R.possibly_equal(builtin_multiply):
                next_var.update(r.vars)
                # if R.vars[1] == agg_var or R.vars[2] == agg_var:
                #     # then this is one of the arguments to the multiplication operator, so we are going to chase
            elif isinstance(r, Aggregator):
                assert aggregator is None or aggregator is r
                aggregator = r
            elif isinstance(r, NDArrayAccess):
                # check that this is a return variable fom the array
                nd_arrays.add(r)


    ret_constraints = info.conjunctive_constraints[self.ret]

    for r in ret_constraints:
        if r.possibly_equal(builtin_multiply):
            pass


    return self


# this needs to be able to scan all of the defined terms and identify is
# something can be written more efficiently as a matrix in which case.
#
# in the case that there are already values defined for some expression which are dense
# and all using integers in some range, then this should identify that as a matrix.
# also if there is a case where
