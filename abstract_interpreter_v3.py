import operator  # used for the aggregator ops
import inspect   # used for assert statements only
from pprint import pprint, pformat  # for demo in main
from collections import defaultdict
import itertools
from functools import reduce

slower_checks = False
try:
    # wall clock: (best) cpython (without slower checks) < pypy < cython < cpython (with slow checks)  ...anyways, it is not that /fast/ even on small problems regardless

    import platform
    slower_checks &= platform.python_implementation() != 'PyPy'  # these checks prevent pypy's jit

    # to make cython work well, would probably require cclass annotations etc on everything
    import cython
    slower_checks &= not cython.compiled

except ImportError:
    pass


use_trace = False  # set to true to get printout of all of the operations

trace_indent = 0
def trace(method, name=''):
    if not use_trace:
        return method
    def wrapped(*args, **kwargs):
        global trace_indent
        print('\t' * trace_indent + f'Calling {name} {method} with {args}')
        trace_indent += 1
        res = method(*args, **kwargs)
        trace_indent -= 1
        print('\t'*trace_indent + f'Returning {name} {method} with {res}')
        if res is method:
            return wrapped
        if callable(res) and res is not done and res is not failure:
            res = trace(res, method.__name__)
        return res
    wrapped.vars = getattr(method, 'vars', None)
    wrapped.children = getattr(method, 'children', None)
    return wrapped

########################################################################################################################
# Base classes and objects used by the interpreter
#
# this includes the frame object which is the current binding of any variables
# and the `done` and `failed` functions which indicate if some operation has failed
#
# Iterators are placed in the frame to indicate that they /would/ bind that variable to one or more ground values if
# they were run

class FBaseType:
    def __call__(self):
        # note that always calling with an empty frame, this is going to have to do rewrites
        # going to have to do rewrites to store values long term

        return self.run(emptyFrame)
    def run(self, frame):
        raise NotImplementedError()
    @property
    def vars(self):
        return ()
    @property
    def children(self):
        return ()
    def disp(self, indent):
        n = self.__class__.__name__
        return f'{indent}{n}('+', '.join(map(str, self.vars)) + ',\n' + ''.join(c.disp(indent + ' '*(len(n) + 1)) for c in self.children) + ')'
    def __repr__(self):
        return self.disp('')
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.children == other.children and
                                   self.vars == other.vars)
    def __hash__(self):
        hv = getattr(self, '_hashcache', None)
        if hv is not None:
            return hv
        hv = (hash(self.__class__) ^
              hash(self.children) ^
              hash(self.vars))
        self._hashcache = hv
        return hv

    def rewrite(self, rewriter=lambda x: x):
        return self
    def rename_vars(self, remap):
        return self.rewrite(lambda c: c.rename_vars(remap))
    def possibly_equal(self, other):
        # for help aligning constraints during optimization to perform rewriting
        # should consider everything except variables names
        return type(self) is type(other) and len(self.vars) == len(other.vars)

    def get_iterators(self):
        # we can't just loop through the structure returning iterators, we need
        # these iterators to be able to /interact/ a bit with eachother.
        # Namely, we need to know if we can iterate something which might
        # require knowing that we can iterate its arguments etc
        ret = defaultdict(set)
        self._get_iterators(ret)
        return ret  # the dict is a map of variables->{set of iterators that can bind it}
    def _get_iterators(self, ret):
        for c in self.children:
            c._get_iterators(ret)

    def refine_and_execute(self, var_map):
        a = self.refine(var_map)
        return a()
    def refine(self, var_map):
        if var_map:
            m = {k:ConstantVariable(k, var_map[k]) for k in var_map}
            return self.rename_vars(lambda x: m.get(x,x))
        return self
    def run_cb(self, callback):
        # callback with the result of this object
        frame, r = self()
        if r is not failure:
            return callback(frame, r)
        return frame, r

class Ffunction(FBaseType):
    def __init__(self, func):
        super().__init__()
        self.func = func
    def run(self, frame):
        return self.func(frame)
    def __eq__(self, other):
        return type(self) is type(other) and self.func is other.func
    def __hash__(self):
        return hash(self.func)
    def disp(self, indent):
        ret = [f'{indent}{self.func.__name__}(\n']
        if self.func.__closure__:
            for c in self.func.__closure__:
                if isinstance(c.cell_contents, FBaseType) and c.cell_contents is not self:
                    ret.append(c.cell_contents.disp(' ' * len(ret[0])) + ',')
        ret[-1] = ret[-1][:-1] + ')'
        return ''.join(ret)


@Ffunction
def done(frame):
    return frame, (done if not frame.isFailed() else failure)

@Ffunction
def failure(frame):
    return failedFrame, failure

@Ffunction
def error(frame):
    # I suppose could instead have an error state that propagates like failure??
    # error could just return itself, and do nothing, so if it isn't eleminated
    # it would eventually propagate into the aggregator if it is the last thing
    # failure would then eleminate the error state all together?

    # we are waiting for something else to eleminate this state
    # so we just keep returning the frame and ourselves
    # the aggregator will have to deal with this eventually
    return frame, error

########################################
# Iterators

def my_hash(obj):
    # make set, dict, list and general iterables hashable
    try:
        return hash(obj)
    except TypeError:
        if isinstance(obj, dict):
            obj = obj.items()
        r = hash(type(obj))
        for v in obj:
            r ^= my_hash(v)
        return r

class Iterator:
    def add_alternate(self, alternate):
        # if there are more than one way to iterate a variable, then we want to track that
        # meaningful in the case that we want to search over the best strategy
        return self
    def bind_iterator(self, frame, variable, value):
        assert not isinstance(value, Iterator)
        # return a new iterator where the value has been set to the value
        r = Frame(frame)
        r[variable] = value
        return r
    def run(self, frame, callback):
        r = Frame(frame)
        it = self._make_iterator()
        try:
            while True:
                v = next(it)
                r.update(v)
                callback(r)  # ignore the returned result
        except StopIteration:
            pass
    def _make_iterator(self):
        raise NotImplementedError()
    def __eq__(self, other):
        return self is other
    def __hash__(self):
        return object.__hash__(self)

class UnionIterator(Iterator):
    def __init__(self, switching_variable, variable, a, b):
        assert isinstance(a, Iterator) and isinstance(b, Iterator)
        self.switching_variable = switching_variable
        self.variable = variable
        self.a = a
        self.b = b
    def bind_iterator(self, frame, variable, value):
        if self.variable == variable:
            aframe = self.a.bind_iterator(frame, variable, value)
            if aframe.isFailed():
                frame = frame.setVariable(self.switching_variable, 2)
                return self.b.bind_iterator(frame, variable, value)
            else:
                bframe = self.b.bind_iterator(aframe, variable, value)
                if bframe.isFailed():
                    return aframe.setVariable(self.switching_variable, 1)
                else:
                    return bframe.setVariable(self.switching_variable, 3)
        elif self.switching_variable == variable:
            sb  = super().bind_iterator(frame, variable, value)
            if value == 3:
                return sb # we still have two branches to handle, don't assign a value to the variable
            elif value == 1:
                return sb.setVariable(self.variable, self.a)
            elif value == 2:
                return sb.setVariable(self.variable, self.b)
        assert False  # should not reach

    def run(self, frame, callback):
        def m1(frame):
            bframe = self.b.bind_iterator(frame, self.variable, frame.getVariable(self.variable))
            # we could just modifiy the frame here instead of copying with setVariable due to how this is written
            # but to just make this conceptually simpler with what state transitions are happening going to use the setVariable
            # method that copies the frame

            if bframe.isFailed():
                # A - B branch
                frame = frame.setVariable(self.switching_variable, 1)
            else:
                # A intersect B branch
                frame = bframe.setVariable(self.switching_variable, 3)
            callback(frame)

        self.a.run(frame, m1)

        def m2(frame):
            aframe = self.a.bind_iterator(frame, self.variable, frame.getVariable(self.variable))
            if aframe.isFailed():
                # B - A branch
                frame = frame.setVariable(self.switching_variable, 2)
                callback(frame)
        self.b.run(frame, m2)
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.switching_variable == other.switching_variable and
                                   self.a == other.a and
                                   self.b == other.b)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.switching_variable) ^ hash(self.a) ^ hash(self.b)

# for wrapping python iterators into our frame aware iterator
class IteratorFromIterable(Iterator):
    def __init__(self, variable, iterable):
        self.iterable = iterable
        self.variable = variable
    def bind_iterator(self, frame, variable, value):
        assert variable is self.variable
        if value in self.iterable:
            return super().bind_iterator(frame, variable, value)
        else:
            return failedFrame
    def _make_iterator(self):
        for v in self.iterable:
            # can potentially return multiple variable -> values in this map at the same time
            yield {self.variable: v}

    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.variable == other.variable and
                                   self.iterable == other.iterable)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.variable) ^ my_hash(self.iterable)

class SingleIterator(Iterator):
    def __init__(self, variable, value):
        self.variable = variable
        self.value = value
    def bind_iterator(self, frame, variable, value):
        assert variable is self.variable
        if value == self.value:
            return super().bind_iterator(frame, variable, value)
        else:
            return failedFrame
    def run(self, frame, callback):
        frame = frame.setVariable(self.variable, self.value)
        if not frame.isFailed():
            callback(frame)
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.variable == other.variable and
                                   self.value == other.value)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.variable) ^ hash(self.value)

class RemapVarIterator(Iterator):
    def __init__(self, remap, wrapped):
        self.remap = remap
        self.wrapped = wrapped
    def run(self, frame, callback):
        return self.wrapped.run(frame, callback)
    def bind_iterator(self, frame, variable, value):
        if self.remap:
            frame = self.wrapped.bind_iterator(frame, self.remap.get(variable, variable), value)
        if not frame.isFailed():
            frame = Frame(frame)
            frame[variable] = value
        return frame
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and
                                   self.remap == other.remap and
                                   self.wrapped == other.wrapped)
    def __hash__(self):
        return hash(type(self)) ^ my_hash(self.remap) ^ hash(self.wrapped)


########################################
# Frame

class ConstantVariable:
    def __init__(self, var, value):
        self.value = value
    # I suppose that if we have two variables that take on the same value, even if they weren't unified together
    # we /could/ consider them the same variable?  There isn't that much of a difference in this case
    def __str__(self):
        return f'={self.value}'
    def __repr__(self):
        return str(self)
    def __eq__(self, other):
        return (self is other) or (type(self) is type(other) and self.value == other.value)
    def __hash__(self):
        return hash(type(self)) ^ hash(self.value)

class Frame(dict):
    def setVariable(self, variable, value):
        assert not isinstance(value, Iterator)
        if isinstance(variable, ConstantVariable):
            if variable.value == value:
                return self
            else:
                return failedFrame
        elif variable in self:
            if self[variable] == value:
                return self
            else:
                return failedFrame
        else:
            f = Frame(self)
            f[variable] = value
            return f

    def getVariable(self, variable):
        if isinstance(variable, ConstantVariable):
            return variable.value
        return self.get(variable, None)

    # short cut for this particular operation
    def isBound(self, variable):
        return isinstance(variable, ConstantVariable) or variable in self

    # need to clear the slot, or maybe we are just trying to be tidy in this implementation
    def remove(self, variable):
        self.pop(variable, None)

    def isFailed(self):
        return False

    def __repr__(self):
        nice = {str(k).split('\n')[0]: v for k,v in self.items()}
        return pformat(nice, indent=1)

    def merge(self, other):
        if not other:
            return self
        if not self:
            return other
        r = Frame(self)
        r.update(other)
        return r

class _EmptyFrame(Frame):
    def __setitem__(self, var, val):
        assert False  # don't set on this frame directly, can return a new instance that will
    # this is an empty dict, so bool(self) == False
    def update(self, *args, **kwargs):
        assert False

emptyFrame = _EmptyFrame()

class _FailedFrame(Frame):
    def setVariable(self, variable, value):
        return self
    def isFailed(self):
        return True
    def remove(self, variable):
        pass
    def __setitem__(self, var, val):
        assert False  # don't set values on the failed frame
    def __repr__(self):
        return '{FailedFrame, ...=...}'

failedFrame = _FailedFrame()

########################################################################################################################
# Main included operations
#
# this includes intersection and union (comma and semicolon) as well as loop which will bring variables down to ground values
# so that in dyna we can aggregate them
#
# The aggregator is split into two parts which differentate between where we compute the final result and the computation
# that takes place inside of the loop on each iteration that add the result to the aggregators internal state

class Intersect(FBaseType):
    def __init__(self, a,b):
        super().__init__()
        assert a and b
        self.a = a
        self.b = b
    def run(self, frame):
        aresult, anew = self.a()
        if aresult.isFailed():
            return aresult, failure
        # we need to refine the rule suffix before we start trying to run it to include results of a
        bresult, bnew = self.b.refine_and_execute(aresult)
        if bresult.isFailed():
            return bresult, failure
        anew = anew.refine(bresult)  # we are not evaluating the result

        # merge the dicts
        bresult = bresult.merge(aresult)

        if anew is self.a and bnew is self.b:
            # return ourselves so that our caller could track that this is a NOP
            # more of an implementation detail than something that is that interesting?
            return bresult, self

        # use the method so that it can return done in the case that both branches are done (don't always create an interescting object)
        return bresult, intersect(anew, bnew)
    def disp(self, indent):
        return f'{self.a.disp(indent)}, {self.b.disp(indent)}'
    @property
    def children(self):
        return self.a, self.b
    def rewrite(self, rewriter=lambda x:x):
        return intersect(rewriter(self.a), rewriter(self.b))

def intersect(a,b,*other):
    if other:
        b = intersect(b, *other)
    if a is done:
        return b
    if b is done:
        return a
    if a is failure or b is failure:
        return failure
    if a is error and b is error:  # propagate the errors up
        return error
    return Intersect(a,b)


class Union(FBaseType):
    def __init__(self, unioned_vars :tuple, a,b, switching_var_name=None):
        super().__init__()
        assert isinstance(unioned_vars, tuple)
        self.unioned_vars = unioned_vars
        self.a = a
        self.b = b
        self.switching_var_name = switching_var_name or ('switching_var', a, b)
    @property
    def vars(self):
        return (self.switching_var_name, *self.unioned_vars)
    @property
    def children(self):
        return self.a, self.b
    def disp(self, indent):
        s = indent + 'union(' + str(self.unioned_vars) + ',\n'
        for c in self.children:
            s += indent + '      ' + c.disp(indent + ' '*6) + ',\n'
        s += indent + ')'
        return s
    def rename_vars(self, remap):
        sv = remap(self.switching_var_name)
        if isinstance(sv, ConstantVariable):
            # just return the branch that was selected by the switching variable
            if sv.value == 1:
                return self.a.rename_vars(remap)
            elif sv.value == 2:
                return self.b.rename_vars(remap)
        return Union(tuple(map(remap, self.unioned_vars)), self.a.rename_vars(remap), self.b.rename_vars(remap),
                     switching_var_name=sv)
    def rewrite(self, rewriter=lambda x:x):
        return Union(self.unioned_vars, rewriter(self.a), rewriter(self.b),
                     switching_var_name=self.switching_var_name)
    def cb_run(self, callback):
        pass
    def _get_iterators(self, ret):
        # we have to get iterators from all of the branches to be able to iterate a variable

        aiters = self.a.get_iterators()
        biters = self.b.get_iterators()
        for v in self.unioned_vars:
            if v in aiters and v in biters:
                # then we can iterate this variable
                for av in aiters[v]:
                    for bv in biters[v]:
                        ret[v].add(UnionIterator(self.switching_var_name, v, av, bv))
    def run_cb(self, callback):
        # we are going to match against the different frames and then call the callback with anything that works
        # this will allow us to implement different matching procedures.  The callback probably comes from the aggregator
        switching_variable = emptyFrame.getVariable(self.switching_var_name)  # this is a bit of a hack, look at the details of frame for more info...
        if switching_variable == 1:
            return self.a.run_cb(callback)
        elif switching_variable == 2:
            return self.b.run_cb(callback)

        aframe, anew = self.a.run_cb(callback)
        bframe, bnew = self.b.run_cb(callback)

        return self.merge_results(emptyFrame, aframe, anew, bframe, bnew)

    def run(self,frame):
        switching_variable = frame.getVariable(self.switching_var_name)
        if switching_variable == 1:
            return self.a()
        elif switching_variable == 2:
            return self.b()

        aframe, anew = self.a()
        bframe, bnew = self.b()

        return self.merge_results(frame, aframe, anew, bframe, bnew)

    def merge_results(self, frame, aframe, anew, bframe, bnew):
        if aframe.isFailed():
            return bframe, bnew
        if bframe.isFailed():
            return aframe, anew

        rframe = Frame(frame)
        for v in self.unioned_vars:
            if v in aframe:
                if v in bframe:
                    if aframe[v] == bframe[v]:
                        rframe[v] = aframe[v]
                    else:
                        # then we have to /store/ the value back into the F object so that it is checked
                        anew = intersect(Constant(v, aframe[v]), anew)
                        bnew = intersect(Constant(v, bframe[v]), bnew)
                else:
                    anew = intersect(Constant(v, aframe[v]), anew)
            elif v in bframe:
                bnew = intersect(Constant(v, bframe[v]), bnew)

        if anew == self.a and bnew == self.b:
            return rframe, self
        elif anew is done and bnew is done:
            return rframe, done
        else:
            return rframe, Union(self.unioned_vars, anew, bnew, switching_var_name=self.switching_var_name)

def union(unioned_vars, a,b, *other):
    if other:
        b = union(unioned_vars, b, *other)

    # if either of the unifications has already succeeded, then we
    # return the one that still needs work done, but we catch the failures
    # as a failure of a branch at this point doesn't mean that
    if a is done and b is done:
        return done

    if a is failure:
        return b
    if b is failure:
        return a

    return Union(unioned_vars, a, b)


class UnboundLoopedVariablesError(RuntimeError):
    def __init__(self, vars, *args):
        super().__init__(*args)
        self.vars = vars

def loop(loops, F, callback, *, bind_all=False):
    # loop represents a strategy for binding variables that are defined in the F
    # we are going to collect iterables from the frame and use those to drive the loop, we are going to recurse
    # calling ourselves on nested versions
    #
    # we are going to bind at least one variable, which corresponds with making progress.  We can bind more than one variable if
    # we can find something that does that
    #
    # callback(frame, F).  Returned value is ignored

    frame, res = F()
    if frame.isFailed():
        return failedFrame, failure

    if all(frame.isBound(v) for v in loops):
        # then there is nothing for us to do
        return callback(frame, res)

    all_loopable = res.get_iterators()  # this is a dict of all possible values that could be looped

    loopable = {k:v for k,v in all_loopable.items() if k in loops}  # select from the variables that we have been instructed to loop

    def run_loop(loopable):
        # choose one of the variables to loop over and start doing that
        loopable = reduce(operator.or_, loopable.values(), set())  # make a set of all the iterators
        l = free_choice(list(loopable))
        if bind_all:
            # then ensure that all of the variables end up bound before calling the callback
            l.run(frame, lambda nframe: loop(loops, res.refine(nframe), callback, bind_all=True))
        else:
            l.run(frame, lambda nframe: callback(*res.refine_and_execute(nframe)))

    if loopable:
        run_loop(loopable)
    else:
        if res is not done and not frame.isFailed():
            # ensure we have propagated everything as we are about to encounter an error
            # saturate_prolog just gives us prolog level constraints at intersection
            frame, res2 = saturate_prolog(frame, res)
            if res2 != res:
                all_loopable = res2.get_iterators()
                loopable = {k:v for k,v in all_loopable.items() if k in loops}  # select from the variables that we have been instructed to loop
                if loopable:
                    # there is now something that we can loop over
                    return run_loop(loopable)

            if res2 is not done and not frame.isFailed():
                # in this case, we are unable to bring all of the head variables to ground
                # which likely indicates that there are constraints that can drive on the variables
                # we would need that aggrgators are willing to propagate the remaining delayed constraints at this time
                # as there is no additional information that is going to be determined
                #
                # we can try prolog style looping, however if the program is written in the standard
                # dyna form (where there is aggregation and consolidation everywhere) then there is probably
                # no unions that we can choose to iterate over
                # if the program is written in /prolog style/ (meaning that we are allowing :- to return the same
                # value multiple times and are _only_ using the intersect and union constructs)
                # then the prolog looping approach should work well.
                # this would basically turn any streams into being unconsolidated
                # I suppose that we could have some rewrite on F which marks different aggregators as having
                # to perform consolidation???
                #
                # Though that would require a closer intergration between aggregators and looping (standard in DDR ;)

                # we already tried saturate_slow_optimize which includes the quote and unify operators
                # which is required to recover prolog behavior, so there isn't anything that

                # by not looking throught the aggregators, this respects "orderly enumeration", but will probably find nothing....
                unions = [f for f in get_intersecting_constraints(res2, (Union, AggregatorOuter)) if isinstance(f, Union)]

                if unions:
                    sv = free_choice(unions).switching_var_name

                    for b in (1,2):
                        loop(loops, res.refine(frame.setVariable(sv, b)), callback, bind_all=bind_all)
                    return frame, done


            # we are going to try building a new index over the variables we still need to enumerate
            # there could be a number of ways this could be done in dyna
            #  1) memoize something as null, but start guessing null now
            #  2) backoff to prolog style execution and try and identify all heads of an expression (though might not terminate...)
            # this will be a demo of how (2) could work
            unbound = tuple(v for v in loops if not frame.isBound(v))
            # if we can't ground the variables, then this will raise an error
            built_index = build_index_over_variables(frame, res2, unbound)
            return run_loop({None:set((built_index,))})

        else:
            return frame, res

def build_index_over_variables(frame, F, vars):
    # the idea here is that we need /some/ index over one of the variables in the head of the expression but it is still free
    # and so we are unable to drive the expression
    # we can switch to a /prolog mode/ where we are going to get unconsolidated streams of values
    #
    # Note, we have to be sure that we have the full set, so while we could in theory use partial results from this point
    # it wouldn't actually save us as we would still have to ensure that we reach the end
    #
    # we can also termiante the prolog solver early once we have all of the variables that we are interested in bound
    # as we don't /care/ if something we get is not valid, we are just looking for possible uper bounds in this case
    #
    #
    # I think there might be a way to /cache/ the results computed during this mode as un-consolidated streams and reuse those later
    # when running with the head bound again.  The idea would basically be only look through /some/ aggregators instead of all of them (at once)
    # and then we could cache the results of anything that we are looking through as an unconsolidated stream, but then let
    # it perform aggregation /further down/ in the F structure in different aggregators that we are _not_ allowing ourselves to look through


    # delete loops and aggregation which prevent us from running :put_sunglasses_on: /prolog style/
    F = make_prolog_style(F)

    ret = set()

    # this is mostly a reimplementation of prolog_driver that terminates early if all of the vars that we are interested in are set
    def drive(frame, F):
        nonlocal ret, vars
        # could use saturate_prolog or saturate_with_optimize here? but those seem to just make this entire thing toooo slow
        # if we can get away without propagating stuff then that can make this faster
        frame, fnew = saturate(frame, F)
        if frame.isFailed():
            return
        if all(frame.isBound(v) for v in vars):
            ret.add(tuple(frame.getVariable(v) for v in vars))
            return
            # then we have identified a possible binding for all of our variables
        # look for any union branches or an iterators that we want
        iterators = reduce(operator.or_, fnew.get_iterators().values(), set())
        loopables = [f for f in get_intersecting_constraints(F) if isinstance(f, Union)] + list(iterators)
        if loopables:
            chosen = free_choice(loopables)
            if isinstance(chosen, Union):
                sv = chosen.switching_var_name
                drive(frame.setVariable(sv, 1), fnew)
                drive(frame.setVariable(sv, 2), fnew)
            else:
                assert isinstance(chosen, Iterator)
                chosen.run(frame, lambda f2: drive(f2, fnew))
        else:
            # there are no unions, maybe we hit a failed state and didn't even know it, so we can retry the slower saturate strategy
            # and see if we can get a failure out otherwise we have to raise the error
            frame, fnew = saturate_with_optimize(frame, fnew)
            if fnew is failure:
                return

            assert not all(frame.isBound(v) for v in vars)
            # there exists a free variable in the set that we are interested in and were unable to bring to ground
            # we need help from the aggregator in this case to be able to consider that whatever is being added to the aggregator
            # would have an infinite contribution
            # if the variable is the final result into the aggregator, then this is like someone wrote `f += X.` which is definitly a user error.
            frees = [v for v in vars if not frame.isBound(v)]
            frees_s = ' '.join([str(v) for v in frees])
            raise UnboundLoopedVariablesError('free variable even after performing prolog style unfication, aggregator has inf contribution: '+frees_s,
                                              frees)
    drive(frame, F)

    # if we made it this far, then we have a set of valid values, so we are going to build an iterator and use that to drive the loops
    # over the values
    class IndexIterator(Iterator):
        def _make_iterator(self):
            for v in ret:
                yield dict(zip(vars, v))

    return IndexIterator()


class AggregatorOuter(FBaseType):
    def __init__(self, ret, head_vars, aggregator, body, *, accumulated_value=None):
        super().__init__()
        assert isinstance(head_vars, tuple)
        self.ret = ret
        self.head_vars = head_vars
        self.aggregator_op = aggregator
        self.body = body
        self.accumulated_value = accumulated_value
    @property
    def children(self):
        return (self.body,)
    @property
    def vars(self):
        return (self.ret, *self.head_vars)
    def disp(self, indent):
        return f'{indent}AggregatorOuter({self.ret},\n{indent}{self.body.disp(indent+" "*16)})'
    def rewrite(self, rewriter=lambda x:x):
        return AggregatorOuter(self.ret, self.head_vars, self.aggregator_op,
                               rewriter(self.body), accumulated_value=self.accumulated_value)
    def rename_vars(self, remap):
        return AggregatorOuter(remap(self.ret), tuple(map(remap, self.head_vars)),
                               self.aggregator_op, self.body.rename_vars(remap),
                               accumulated_value=self.accumulated_value)
    def combine(self, a,b):
        if a is None:
            return b
        if b is None:
            return a
        return self.aggregator_op(a,b)

    def run(self, frame):
        accumulated_value = self.accumulated_value
        # in the case of a ground query against this expression it is _not_ ok to return a non-ground
        # results, so we have to bring the query to ground such that
        ground_query = all(frame.isBound(v) for v in self.head_vars)

        def callback(frame, F):
            nonlocal accumulated_value, ground_query
            assert isinstance(F, AggregatorInner) or (F is done and accumulated_value is not None)

            if F is done:
                return frame, done
            elif F.body is done:
                # then this rule has finished, we can add the result into the aggregator
                accumulated_value = self.combine(accumulated_value, frame.getVariable(F.readVar))
                return frame, done
            elif ground_query:
                # then we are not allowed to have the body be non-ground.  We
                # need to call out and loop the variables that are in the body
                # to get a ground result
                loop(F.introduced_vars, F, callback)
                return frame, done
            else:
                # then we are unable to aggregate this result, and there are still delayed constraints
                # so we are just going keep this object around ___ IN THE UNION ___ rather than
                # making the aggregator responsible for handling this disjunction
                return frame, F

        frame, res = self.body.run_cb(callback)
        if res is done:
            # then the aggregator has finished, so we are going to return the result of aggregation
            # along with the value
            return frame.setVariable(self.ret, accumulated_value), done
        elif frame.isFailed():
            assert res is failure
            return frame, res  # the result is null
        if accumulated_value != self.accumulated_value or res != self.body:
            return frame, AggregatorOuter(self.ret, self.head_vars, self.aggregator_op, res, accumulated_value=accumulated_value)
        return frame, self

aggregator_outer = AggregatorOuter

class AggregatorInner(FBaseType):
    # track the reference of what variable is the returned result
    # this is also going to work as a /container/ for the
    def __init__(self, readVar, introduced_vars, body):
        super().__init__()
        self.readVar = readVar
        # these are variables that we would have to drive a loop over if we want to bring this expression to ground
        # these were previously on the Loop() object, but given that we are getting rid of that, this information still has to go /somewhere/
        self.introduced_vars = introduced_vars
        self.body = body
    @property
    def vars(self):
        return (self.readVar,)
    @property
    def children(self):
        return (self.body,)
    def disp(self, indent):
        return f'AggregatorInner({self.readVar},\n{indent+" "*16}{self.body.disp(indent+" "*16)})'
    def rewrite(self, rewriter=lambda x:x):
        return AggregatorInner(self.readVar, self.introduced_vars, rewriter(self.body))
    def rename_vars(self, remap):
        return AggregatorInner(remap(self.readVar), tuple(map(remap, self.introduced_vars)), self.body.rename_vars(remap))
    def run(self, frame):
        frame, res = self.body()
        if res is self.body:
            return frame, self
        if res is failure:
            return failedFrame, failure

        return frame, AggregatorInner(self.readVar, self.introduced_vars, res)

aggregator_inner = AggregatorInner


def saturate(frame, F):
    # As written, the current system does not ensure that all of the constraints are aware of the most recent set values
    # this runs the F function in a loop until it indicates that it has converged and there is nothing more for it to do
    r = Frame(frame)
    while True:
        last_F = F
        F = F.refine(frame)
        frame, F = F()
        if frame.isFailed():
            return frame, F
        r = r.merge(frame)
        if F is last_F:  # we must have reached a fixed point of the continuations
            break
    return r, F


########################################################################################################################
# the above methods I would consider part of the core
# the methods below are for writing simple wrappers
# for things like builtins which have modes
# also common operators like quote and runtime unify, and constraints for constants

class ModedOp(FBaseType):
    def __init__(self, name, ops, vars):
        self.vars_ = vars
        self.ops = ops
        self.name = name
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

def moded_op(name, op):
    arity = max(map(len, op.keys()))
    assert arity == min(map(len, op.keys()))
    def method_locations(*locs):
        assert len(locs) == arity
        return ModedOp(name, op, locs)
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
    if slower_checks:
        arity = len(inspect.getfullargspec(op).args)
    def method_locations(*locs):
        if slower_checks:
            assert len(locs) == arity
        return CheckOp(name, op, locs)
    return method_locations

class Constant(FBaseType):
    def __init__(self, var, value):
        super().__init__()
        self.var = var
        self.value = value
    @property
    def vars(self):
        return (self.var,)
    def run(self, frame):
        return frame.setVariable(self.var, self.value), done
    def _get_iterators(self, ret):
        ret[self.var].add(SingleIterator(self.var, self.value))
    def disp(self, indent):
        return f'{self.var}={self.value}'
    def __eq__(self, other):
        return type(self) is type(other) and self.var == other.var and self.value == other.value
    def __hash__(self):
        return hash(self.var) ^ hash(self.value)
    def rename_vars(self, remap):
        return Constant(remap(self.var), self.value)
    def possibly_equal(self, other):
        return type(self) is type(other) and self.value == other.value

constant = Constant


class Quote(FBaseType):
    def __init__(self, name, res, *args):
        super().__init__()
        self.name = name
        self.res = res
        self.args = args
    @property
    def vars(self):
        return (self.res, *self.args)
    def run(self, frame):
        ret = frame
        if frame.isBound(self.res):
            try:
                # then we are going to try and unbox the variable
                bname, *vals = frame.getVariable(self.res)
            except TypeError:
                return failedFrame, failure
            if bname != self.name or len(vals) != len(self.args):  # wrong name on the tuple, unification failed
                return failedFrame, failure
            for l, v in zip(self.args, vals):
                ret = ret.setVariable(l, v)
            return done.run(ret)  # done will return failure in the case the frame is failed
        sic = sum(frame.isBound(l) for l in self.args)
        if sic == len(self.args):
            return frame.setVariable(self.res, (self.name, *(frame.getVariable(l) for l in self.args))), done

        # TODO: need something that can /product/ multiple iterators together to construct the final quoted result as the output
        # that would basically be some quote aware iterator, or something that needs to run a constraint after running the iterators
        # then that constraint could
        # that would get the neural network on multiple dimention weights
        return ret, self
    def _get_iterators(self, ret):
        pass
        # if False:  # we can copy the iterator between quote and its elements, might be faster idk...
        #            # due to loops fallback mechnisms (for building an index) it doesn't actually extend /what/ we can run

        #     if isinstance(frame.getVariable(self.res), Iterator):
        #         # then we can iterate the result variable, so we can copy that iterator into all of our args that currently are empty
        #         ri = frame.getVariable(self.res)
        #         for a in self.args:
        #             if frame.getVariable(a) is None:
        #                 ret = ret.setVariable(a, RemapVarIterator(None, ri))  # we can't use this to directly perform the exclusion checks....
        #     if sic == len(self.args) -1:
        #         # then there is only 1 value that is unbound in the tuple, and if we can iterate it, so just copy the iterator to the final result
        #         for a in self.args:
        #             if isinstance(frame.getVariable(a), Iterator):
        #                 ret = frame.setVariable(self.res, RemapVarIterator(None, frame.getVariable(a)))

    def disp(self, indent):
        return f'{self.res}=&{self.name}(' + ', '.join(map(str, self.args)) + ')'
    def rename_vars(self, remap):
        return Quote(self.name, remap(self.res), *map(remap, self.args))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.name == other.name and len(self.args) == len(other.args)

quote = Quote

class Unify(FBaseType):
    def __init__(self, a, b):
        assert a is not b
        self.a = a
        self.b = b
    @property
    def vars(self):
        return self.a, self.b
    def run(self, frame):
        mode = (frame.isBound(self.a), frame.isBound(self.b))
        av = frame.getVariable(self.a)
        bv = frame.getVariable(self.b)

        # check if the values are equal or copy the ground value
        if mode == (True, True):
            if av == bv:
                return frame, done
            else:
                return failedFrame, failure
        elif mode == (True, False):
            return frame.setVariable(self.b, av), done
        elif mode == (False, True):
            return frame.setVariable(self.a, bv), done

        # if there is an iterator on one of the slots, copy that between eachother
        # still will need to check the values of the variables later though
        if av is None and isinstance(bv, Iterator):
            return frame.setVariable(self.a, RemapVarIterator({self.a: self.b}, bv)), self
        elif bv is None and isinstance(av, Iterator):
            return frame.setVariable(self.b, RemapVarIterator({self.b: self.a}, av)), self

        return frame, self
    def rename_vars(self, remap):
        a = remap(self.a)
        b = remap(self.b)
        if a == b:
            return done
        return Unify(a,b)

    def _get_iterators(self, ret):
        if self.a in ret:
            ret[self.b] |= ret[self.a]
        if self.b in ret:
            ret[self.a] |= ret[self.b]

eq = Unify

class NotEqual(FBaseType):
    def __init__(self, a,b):
        assert a is not b
        self.a = a
        self.b = b
    @property
    def vars(self):
        return self.a, self.b
    def run(self, frame):
        if frame.isBound(self.a) and frame.isBound(self.b):
            if frame.getVariable(self.a) == frame.getVariable(self.b):
                return failedFrame, failure
            else:
                return frame, done
        return frame, self
    def rename_vars(self, remap):
        a = remap(self.a)
        b = remap(self.b)
        if a == b:
            return failure
        return NotEqual(a, b)

not_equal = NotEqual



########################################################################################################################
# everything above this line should deal with some internal representation about the interpreter




# the "returned" value is the first argument
# because we are representing the expressions as being attached to variables, there is not "proper" return variable
# instead I am just using this as a convention in this file
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

int_v = check_op('int', lambda x: isinstance(x, int))



########################################################################################################################
# this is for the interpreter to call the IR-Dyna (result of parsing the ASTs)
# we have to first give all variables a unique name (as the frame itself is flag) via localVar
# the recursive_callable can go on every method to just be safe, but it _must_ go on any method that is recursive
# it makes it such that when evaluating a recursive method, that when we hit it a second time, we first check
# the stack to ensure that it is doing something "unique" and just just running endlessly.
# that will essentially prevent if from just inlining the all free case all the way down
#  This doesn't prevent recursion that was actually intended to run forever, just things like `a(X) :- a(X).` or
#  case where some other constraint is going to cut of the branch.



def localVar(name='_'):
    # just create unique names by constructing an object which hashes uniquly and is only equal to itself
    if slower_checks:
        # nicer for printing, but not required
        return (inspect.getouterframes(inspect.currentframe())[1].function,
                name, object())
    else:
        return (name, object())

external_called_stack = []

class ExternalCallWrapper(FBaseType):
    # when using one of the optimizing saturate methods, this can run forever.
    # I believe that by identifying aliased variables, it can cause the modes of expressions to apparently change,
    # and thus this is willing to continue expanding an expression, assuming that it is in the wrong mode
    def __init__(self, method, args, parent_args):
        super().__init__()
        self.method = method
        self.args = args
        self.parent_args = parent_args
        self.cache = None
        self.force_include = False
    def can_run(self):
        identical_calls_found = 0
        # look through the frame and see if we can find anything that matches this method
        for p in self.parent_args:
            # then we have found a frame that has the same variable value pattern as us
            # this means that it must be the case that it was the same variable (so the value was just getting passed down)
            # or that both were unbound

            # worked example of `a(X) :- a(X+1), X < 5.`  This will work and terminate
            # in prolog that would just run forever due to not checking the second constraint.
            # in this case, we are going to start with a(-X) being free, we can then make `Y=add(X,1), a(-Y)` because we can't evaluate
            # the constraint, so it is also free (just like the parent) in which case it will just remain a delayed constraint
            # that is not yet inlined or checked.
            # for `X=0, a(+X)` case with a ground argument, we start with by making a call we then end up with `Y=add(X, 1), a(Y), X<5`
            # at which point `a(-Y)` is being abstractly evaluated and it does not match its parent of `a(+0)` so it gets inlined
            # however then we end up back in the above case of `a(-X)` which terminates.  So we end up with basically 1 extra level of
            # `a` inlined.  At that point, there is just going to be a delayed constraint that calls out to `a(-Z)` and will need to be checked
            # later but yields control flow to something else.  Eventually the `X<5` constraint will cut this off this branch
            # and we will stop evaluating this chain / terminate.
            #
            # we can think of this as limiting the /depth/ that we search in a constraint proof before we start looking for other
            # constraints to check.  The limiation being that if we can find something with the same free-value patern higher on the stack
            # then we know for certain that including ourselves would just hit that same state again, so this limits unwanted recursion.
            #
            # the expression `a(X) :- a(X+1).` won't termiante, but will only expand its "inlining depth" by 1 during each application of `F(frame)`
            # so other parts of the program might still be able to make progress...

            if all(emptyFrame.getVariable(pv) == emptyFrame.getVariable(a) for pv, a in zip(p, self.args)):
                if all(emptyFrame.isBound(pv) for pv in p):
                    # in this case we could either return true or false depending on what semantics we want for the language
                    # basically this is the case `a(X) :- a(X).`

                    # in prolog, we can just return that this is failed, and then we can look for other branches for which this statement
                    # is going to work.
                    # in dyna, we would need to memoize that we have been in this state before (or mark it in a table)
                    # that would be a bit like XSB which allows for the recursion to be delayed in trying to figure out what is valid
                    #
                    # this state should be easy to look up in a memoization table, as all of the arguments are ground at this point
                    #
                    # I suppose that we could also allow for non ground expressions to be looked up in the memoization table
                    # but that would potentially require identifying which parts of the entire expression are relevant??? best not to do that
                    return failedFrame, failure
                identical_calls_found += 1
        return identical_calls_found < 1 or self.force_include
    def run(self, frame):
        if self.cache:
            return self.cache()

        if self.can_run():
            try:
                external_called_stack.append(self)
                self.cache = self.method(*self.args)
            finally:
                assert external_called_stack[-1] is self
                del external_called_stack[-1]
            return self.cache()
        else:
            # delay calling this method as the arguments match something that is already higher on the stack
            # it must be the case that there would have been nothing better that we can do at this time
            #
            # by returning ourselves, we are letting the runtime try other constraints which might propagate further in the rule
            # hopefully on the next time that we are called, the parent frame will have learned about the value of one of its arguments
            # and thus maybe done something worthwhile
            return frame, self
    def _get_iterators(self, ret):
        if self.cache:
            self.cache._get_iterators(ret)
        else:
            self.run(emptyFrame)
            if self.cache:
                self.cache._get_iterators(ret)
    @property
    def vars(self):
        return self.args
    @property
    def children(self):
        return ()
    def disp(self, indent):
        return f'{self.method.__name__}(' + ', '.join(map(str, self.args)) + ')'
    def __eq__(self, other):
        return type(self) is type(other) and self.method is other.method and self.args == other.args and self.parent_args == other.parent_args
    def __hash__(self):
        return hash(self.method) ^ hash(self.args)
    def rename_vars(self, remap):
        return ExternalCallWrapper(
            self.method,
            tuple(map(remap, self.args)),
            set(tuple(map(remap, s)) for s in self.parent_args)
        )

def recursive_callable(method):
    # the goal of this method is to ensure that we do not recurse forever
    # while still letting it do early inclusion.  The idea being that we check if the arguments are ground
    # and not being passed around with the same values.
    #
    # Reason for writing this, I think that with it, we /might/ be able to claim to be a super set of prolog?
    # as this basically prevents calling the method in the case where it seems that no more additional useful information would be caputred
    # which if was to be run in a prolog system would proveable just run forever
    # (as it would be hitting the _exact_ same state as something earlier on the stack)

    arity = len(inspect.getfullargspec(method).args)
    def func(*args):
        assert len(args) == arity
        parents = set()
        # look for parent frames of this method and capture the variables

        # for py_frames in inspect.getouterframes(inspect.currentframe()):
        #     pself = inspect.getargvalues(py_frames.frame).locals.get('self', None)
        for pself in reversed(external_called_stack):
            assert isinstance(pself, ExternalCallWrapper)
            if pself.method is method:
                # then this is a parent frame that we are interested in
                # get its parents arguments as well as its own (so if there is some alternating recursion)
                parents |= pself.parent_args
                parents.add(pself.args)
        return ExternalCallWrapper(method, args, parents)
    return func

########################################################################################################################
# optimization passes that we can perform on the program
# and other examples of getting things like the <I, >J, int constraints together to add a range constraint
#
# ideally these would all be /stable/ so calling them again would not change the result,
# but it seems that is not always the case.... so some saturate_slow_optimize seem to maybe not terminate????

class OptionalFailFast(FBaseType):
    def __init__(self, wrapped):
        self.wrapped = wrapped
    @property
    def children(self):
        return (self.wrapped,)
    def run(self, frame):
        # randomly choose if it wants to run it?
        pass
    def disp(self, indent):
        return f'opt({self.wrapped.disp(indent+"    ")})'
    def _get_iterators(self, ret):
        # in the case of getting iterators, we are already allowed to pick between these
        # so we are just going to always include these objects
        self.wrapped._get_iterators(ret)


def get_intersecting_constraints(F, ignore=Union):
    # get all of the constraints that are known to intersect
    # basically anything not covered by a union
    yield F
    if not isinstance(F, ignore):
        yield from (cc for c in F.children for cc in get_intersecting_constraints(c, ignore))

def map_constraints_to_vars(*Fs):
    if not Fs:
        return {}
    ret = defaultdict(list)
    for f in Fs:
        for v in f.vars:
            ret[v].append(f)
        for v, ff in map_constraints_to_vars(*f.children).items():
            ret[v] += ff
    return ret


def make_prolog_style(F):
    # rewrite the program removing all aggregation and nested loop expressions
    # the program might not work, as the results of aggregation are not going to be present anymore
    # might only want to do this partially so we can still get the results of /some/ aggregation
    #
    # if the program is all `:-`, then it should be fine

    # if isinstance(F, Loop):
    #     return make_prolog_style(F.continuation)
    if isinstance(F, (AggregatorInner, AggregatorOuter)):
        return make_prolog_style(F.body)
    elif isinstance(F, ExternalCallWrapper):
        @Ffunction
        def wrappedExtCW(frame):
            frame, r = F(frame)
            if r == F:
                return frame, wrappedExtCW
            return frame, make_prolog_style(r)

        return wrappedExtCW
    else:
        return F.rewrite(make_prolog_style)


def optimize_aliased_vars(F, frame=None):
    # this will rewrite `foo(X, Y), X=Y` to `X=Y, foo(X,X)`

    varF = map_constraints_to_vars(*get_intersecting_constraints(F))
    # first we are going to look for variables that have a unify operation on them
    # and rewrite the expression such that one of them is used everywhere and the other
    # just has the unify expression on it
    alias_vars = defaultdict(set)
    for var, Fs in varF.items():
        for f in Fs:
            if isinstance(f, Unify):
                assert f.a is not f.b
                alias_vars[f.a].add(f.b)
                alias_vars[f.b].add(f.a)

    for var in alias_vars:
        alias_vars[var].add(var)
    done_progagate = False
    while not done_progagate:
        done_progagate = True
        for var in alias_vars:
            for v in alias_vars[var]:
                if alias_vars[var] != alias_vars[v]:
                    r = alias_vars[v] | alias_vars[var]
                    alias_vars[v] = r
                    alias_vars[var] = r
                    done_progagate = False

    for var in list(alias_vars):
        if all(isinstance(f, Unify) for f in varF[var]):
            del alias_vars[var]  # delete things that are already just a single constraint with the unify object, we don't want to add these again..

    do_renames = {}
    for var, Fs in varF.items():
        if var in alias_vars:
            mn = max(alias_vars[var] | set((var,)), key=lambda v: str(v))  # use string name to try and be a bit stable???
            if mn != var:
                do_renames[var] = mn

    # nothing to do
    if not do_renames:
        return F

    done_progagate = False
    while not done_progagate:
        done_progagate = True
        for k, v in list(do_renames.items()):
            if v in do_renames:
                done_progagate = False
                do_renames[k] = do_renames[v]
            if k == v:
                done_progagate = False
                del do_renames[k]

    if not do_renames:
        return F

    # generate a new F with all of these variables renamed
    F = F.rename_vars(lambda v: do_renames.get(v, v))
    for k, v in do_renames.items():
        if frame is None or (not frame.isBound(k) or not frame.isBound(v) or frame.getVariable(k) != frame.getVariable(v)):
            F = intersect(Unify(k, v), F)
    return F


def add_const_to_vartype(var_types, name, const):
    cnames = tuple(('_tmp_check', object()) for _ in range(len(const)-1))
    var_types[name] = (const[0], cnames)
    for v, n in zip(const[1:], cnames):
        if isinstance(v, tuple):
            add_const_to_vartype(var_types, n, v)

def get_variable_types(constraints, var_types=None):
    var_types = var_types or {}
    Fs = [f for f in constraints if isinstance(f, (Constant, Quote))]
    for const in Fs:
        if isinstance(const, Constant) and isinstance(const.value, tuple):
            add_const_to_vartype(var_types, const.var, const.value)
    for q in Fs:
        if isinstance(q, Quote) and q.res not in var_types:
            var_types[q.res] = (q.name, q.args)
    return var_types


def optimize_quote_equivalence(F, var_types=None):
    # if you have `X=&foo(A, B), X=&foo(D, E)`, this will rewrite to `X=&foo(A, B), A=D, B=E`
    var_types = var_types or get_variable_types(get_intersecting_constraints(F))
    quoteF = [f for f in get_intersecting_constraints(F) if isinstance(f, Quote)]
    # identify types of constants on the outer level
    # a fully ground quote operation is viewed as a constant, which is why need to add this

    headQ = {}
    delete_quotes = []
    alias_vars = []

    def check_parent_types(q, pn):
        # check that the types on the `X=&foo()` matches that of its parents
        # this recurses on the abstract type information of the variable list matching name and arity at
        # every point until it hits a free variable

        if pn not in var_types:
            return True  # hit a free (untyped) variable

        n, v = var_types[pn]
        if q[0] != n or len(v) != len(q[1]):
            return False
        if q[1] is v:  # same object, no need to recurse
            return True
        # recurse and check child types
        return all((check_parent_types(var_types[a], p) if a in var_types else True)
                   for a,p in zip(q[1], v))
    for q in quoteF:
        # typechecking on the quote expressions has failed
        if not check_parent_types((q.name, q.args), q.res):
            return failure

        if q.res in headQ:
            # then we are going to unify these variables in the bodies together
            o = headQ[q.res]
            if o.name != q.name or len(q.args) != len(o.args):
                # these two expressions have a different name/arity pair, which means that this /intersected/ F expression
                # is failed, so we are just going to return that early
                return failure
            for a,b in zip(q.args, o.args):
                alias_vars.append(Unify(a,b))
            delete_quotes.append(q)  # not needed any more
        else:
            headQ[q.res] = q

    if delete_quotes:
        def rewriter(f):
            if isinstance(f, Union):  # don't look past unions
                return f
            if isinstance(f, Quote) and f in delete_quotes:
                return done
            return f.rewrite(rewriter)
        F = rewriter(F)
    if alias_vars:
        alias_vars.append(F)
        F = intersect(*alias_vars)
    return F


infered_constraints = [
    # infer the range constraint
    (((int_v('v'), gteq('v', 'a'), lt('v', 'b'))),
     range_v('v', 'a', 'b')),
    (((int_v('v'), gt('v', 'a'), lt('v', 'b'))),  # technically there should be some a+1 for the range constraint,
     range_v('v', 'a', 'b')),                     # but we can add the constraint as it is an upper bound

    # infer a < b < c => a < c
    ((lt('a', 'b'), lt('b', 'c')),
     lt('a', 'c')),
    ((lteq('a', 'b'), lt('b', 'c')),
     lt('a', 'c')),
    ((lteq('a', 'b'), lteq('a', 'c')),
     lteq('a', 'c')),

    ((lt('a', 'b'), lt('b', 'a')),
     failure),
    ((lteq('a', 'b'), lteq('b', 'a')),
     Unify('a', 'b'))
]

def optimize_infered_constraints(F, parent_intersecting=set()):
    # this is for things like I<J<K, J:int => range(J, I, K)

    # there is definitly a much better implementation of this algorithm
    # something that hashes the constraint types and then looks up constraints using filtering or something

    Fset = set(f for f in get_intersecting_constraints(F) if not isinstance(f, Union))
    self_vars = set(map_constraints_to_vars(*Fset).keys())
    varF = map_constraints_to_vars(*(Fset | set(p for p in parent_intersecting if set(p.vars).intersection(self_vars))))

    adding = []

    def align_op(vmap, matching, generating, var_constraints):
        if not matching:
            # done matching this constraint, add in the new infered constraint
            ad = generating.rename_vars(vmap.get)
            if ad not in Fset and ad not in adding and ad not in parent_intersecting:
                adding.append(ad)
        else:
            m, *res = matching
            nvmap = dict(vmap)
            for c in var_constraints:
                if m.possibly_equal(c):
                    # assume that the order in the variables expression are the same between the expressions
                    for km, kc in zip(m.vars, c.vars):
                        if km in nvmap:
                            if nvmap[km] != kc:
                                break
                        else:
                            nvmap[km] = kc
                    else: # meaning that we didn't break the loop
                        assert m.rename_vars(nvmap.get) == c  # check these are now equal
                        align_op(nvmap, res, generating, var_constraints)


    for v, Fs in varF.items():
        # see if we can align the constraints between the two expressions
        for matching, generating in infered_constraints:
            align_op({}, matching, generating, Fs)

    if adding:
        F = intersect(F, *adding)
    return F

def optimize_deduplicate_constraints(F):
    seen = set()
    Fs = (f for f in get_intersecting_constraints(F) if not isinstance(f, Union))
    dd = []
    for f in Fs:
        if f in seen and f not in (done, failure, error):
            dd.append(f)
        else:
            seen.add(f)
    # then we have duplicates, so we are going to delete them
    if dd:
        dd = {d:d for d in dd}
        def rewriter(f):
            if isinstance(f, Union):
                return f
            if f is dd.get(f):  # this needs to use `is` to check it is the same object, not `==`
                return done
            return f.rewrite(rewriter)
        F = rewriter(F)
    return F


def optimize_pull_equal_constraints_up(F):
    # look for equivalent constraints between two branches of a union and lift it up a level

    def get_constraints_from_both_branches(F):
        assert isinstance(F, Union)
        return set(get_intersecting_constraints(F.a)) & set(get_intersecting_constraints(F.b))

    def urewriter(parent_intersecting, F):
        if isinstance(F, Union):
            # first recurse into the children such that it might propagate multiple levels
            F2 = F
            ai = set(get_intersecting_constraints(F.a)) | parent_intersecting
            bi = set(get_intersecting_constraints(F.b)) | parent_intersecting

            af = F.a.rewrite(lambda x: urewriter(ai, x))
            bf = F.b.rewrite(lambda x: urewriter(bi, x))

            if af != F.a or bf != F.b:
                F2 = Union(F.unioned_vars, af, bf, F.switching_var_name)

            c = (set(get_intersecting_constraints(af)) & set(get_intersecting_constraints(bf))) - parent_intersecting

            if c:
                F2 = intersect(F2, *c)
        else:
            F2 = F.rewrite(lambda x: urewriter(parent_intersecting, x))

        if F2 == F:
            return F
        return F2

    intersect = set(get_intersecting_constraints(F))
    return urewriter(intersect, F)

def optimize_identify_useless_unions(F):
    # if there is some union which can't possibly work, then it would fail, so we check if we can achive a failure
    # state by forcing one of the union branches to be either a or b
    # this would probably be more powerful if it was working with whatever frame was actually active as it could consider
    # the values of expression that have already been evaluated

    unions = [u for u in get_intersecting_constraints(F) if isinstance(u, Union)]
    replace_unions = {}
    for u in unions:
        Fp = intersect(u.a, F)
        # these may require more than one pass of optimization / propagation of abstract constraints to identify that these are failed
        # optimizing these all the way to a fixed point would be potentially slow???
        a_failed = optimize_intersecting(Fp) is failure
        Fp = intersect(u.b, F)
        b_failed = optimize_intersecting(Fp) is failure

        # both branches of this union are useless
        if a_failed and b_failed:
            return failure
        elif a_failed:
            replace_unions[u] = u.b
        elif b_failed:
            replace_unions[u] = u.a

    if replace_unions:
        def urewriter(f):
            if isinstance(f, Union):
                return replace_unions.get(f, f)
            r = f.rewrite(urewriter)
            return f if r == f else r
        F = urewriter(F)
    return F

def optimize_intersecting(F):
    F = optimize_aliased_vars(F)
    if F is failure: return F
    F = optimize_quote_equivalence(F)
    if F is failure: return F
    F = optimize_infered_constraints(F)
    return F

def optimize_full(startF, frame=None):
    # sometimes information escapes into the frame, which would be helpful for optimizing the expression
    # essentially when a constant is evaluated, and there is now no record of the type inside of the F expression


    # recurse into unions and optimize each intersecting set of constraints
    def rewriter(parent_intersecting, var_types, startF):
        print('--')
        F = optimize_aliased_vars(startF, frame)
        if F != startF: print('alias')
        if F is failure: return F
        var_types = dict(var_types)
        var_types = get_variable_types(get_intersecting_constraints(F), var_types)
        F = optimize_quote_equivalence(F, var_types)
        if F != startF: print('quote')
        if F is failure: return F
        F = optimize_infered_constraints(F)
        if F != startF: print('infer')
        if F is failure: return F
        F = optimize_identify_useless_unions(F)
        if F is failure: return F
        if F != startF: print('useless')


        children_unions = [f for f in get_intersecting_constraints(F) if isinstance(f, Union)]
        if children_unions:
            # check if we want to rewrite inside of those unions
            # this is pulling constraints down to see if it can use those to infer something in a lower down union context
            parent_intersecting = parent_intersecting | set(f for f in get_intersecting_constraints(F) if not isinstance(f, Union))
            new_children_unions = [
                Union(cu.unioned_vars,
                      rewriter(parent_intersecting, var_types, cu.a),
                      rewriter(parent_intersecting, var_types, cu.b),
                      cu.switching_var_name)
                for cu in children_unions
            ]

            rp = zip(children_unions, new_children_unions)
            rp = {k:v for k,v in rp if not (k == v)}  # check that they are different

            if rp:
                # then we are going to replace the union objects with our rewritten union object that contains a new child
                def urewriter(f):
                    if isinstance(f, Union):
                        return rp.get(f, f)
                    return f.rewrite(urewriter)
                F = urewriter(F)
        return F

    F = optimize_pull_equal_constraints_up(startF)
    if startF != F: print('pullup')

    var_types = {}
    if frame:
        for k, v in frame.items():
            if isinstance(v, tuple) and k != '__active_union_vars':
                add_const_to_vartype(var_types, k, v)

    return rewriter(set(), {}, F)


def saturate_slow_optimize(frame, F):
    while True:
        last_F = F
        F = optimize_full(F, frame)
        frame, F = F(frame)
        if F == last_F: break
    return frame, F

def saturate_with_optimize(frame, F):
    while True:
        while True:
            last_F = F
            F = F.refine(frame)
            frame, F = F()
            if F == last_F: break
        if F in (done, failure, error): break
        F = optimize_full(F)
        if F == last_F: break
    return frame, F


########################################
# heuristics which would be things that aren't "stable" in that they return a different F every time
# or the best F to return isn't clear (somewhere that learning could be applied)

def heuristic_force_include_intersecting_calls(F):
    # identify any external calls that are interesecting and mark them to be included on the next run
    # heuristic with say less than 100 parents? (so that `a(X) :- a(X).` only includes 100 times???)
    did_replace = False
    def rewriter(f):
        nonlocal did_replace
        if isinstance(f, ExternalCallWrapper):
            if len(f.parent_args) < 100:
                n = ExternalCallWrapper(f.method, f.args, f.parent_args)
                n.force_include = True
                did_replace = True
                return n
        elif isinstance(f, Union):
            return f  # do not rewrite inside of the union
        return f.rewrite(rewriter)
    F2 = rewriter(F)
    if did_replace:
        return F2
    return F

def heuristic_reorder_intersecting_constraints(F):
    # we should be able to reshuffle interesting constraints in any order that we want, this can
    # impact the solver's runtime performance as we basically run through the intersecting constraints in order
    # until it converges.  So if we have `A=1, A=B, C=B, D=C.` it would be fast and get it on the first pass where as if we have
    # `C=D, C=B, B=A, A=1` it would only push the value up one step each pass through the list and thus take 4 times longer

    def get_intersect_children(f):
        if isinstance(f, Intersect):
            yield from get_intersect_children(f.a)
            yield from get_intersect_children(f.b)
        else:
            yield f
    intersecting = list(get_intersect_children(F))
    return intersect(*free_reorder(intersecting))

########################################################################################################################
# for making this representation into a Prolog style interpreter we have to run with a different strategy
# namely, that we are going to pick a branch and then try and see if it works.
# additionally we need to

def saturate_prolog(frame, F):
    while True:
        last_F = F
        # for prolog base saturation, we require these extra /optimizations/ which look at the delayed
        # constraint store and determine which variables are aliased together
        F = F.refine(frame)
        F = optimize_aliased_vars(F)
        F = optimize_quote_equivalence(F)

        frame, F = F()
        if F == last_F: break
    return frame, F

def prolog_driver(F, callback, *, look_through_aggregators=False):
    @Ffunction
    def f(frame):
        frame, fnew = saturate_prolog(frame, F)
        if frame.isFailed():
            return frame, failure

        if look_through_aggregators:
            unions = [f for f in get_intersecting_constraints(fnew) if isinstance(f, Union)]
        else:
            unions = [f for f in get_intersecting_constraints(fnew, (Union, AggregatorOuter)) if isinstance(f, Union)]
        if unions:
            sv = free_choice(unions).switching_var_name

            nf = prolog_driver(fnew, callback)
            frame1, r1 = nf(frame.setVariable(sv, 1))
            frame2, r2 = nf(frame.setVariable(sv, 2))
            # if either frame as failed, then we ca just leave the switching variable and have eleminated that branch
            # from this result
            if frame1.isFailed():
                return frame2, r2
            if frame2.isFailed():
                return frame1, r1
            return frame, nf
        else:
            # there are no unions for us to branch over in prolog, so just callback with whatever the frame / delayed remaining constraints are
            return callback(frame, fnew)
    return f


########################################################################################################################
# called where there is some choice

def free_choice(options):
    return options[0]  # prolog style in return the first item

    # try this "dyna" style where we randomly choose one of the possible branches (similar to out of order subgoals)
    # but we are using the constraints to propagate information about the failures
    import random
    return random.choice(options)


def free_reorder(lst):
    import random
    lst = list(lst)
    random.shuffle(lst)
    return lst


########################################################################################################################


def callback_to_iterator(func):
    # due to the technical differences between iterators and callbacks, we need this nasty method
    # only one of these two threads is ever running at a time, so not a big issue...
    import threading
    send = threading.Semaphore(0)
    recv = threading.Semaphore(0)
    done = False
    value = None
    def callback(v):
        nonlocal value, done, send, recv
        value = v
        recv.release()
        send.acquire()
        if done:
            raise KeyboardInterrupt()
        return value
    def threadF():
        nonlocal value, done, send, recv
        send.acquire()
        try:
            if not done:
                func(callback)
        except KeyboardInterrupt:
            pass
        finally:
            done = True
            recv.release()

    def iterator():
        nonlocal value, done, send, recv
        try:
            while True:
                send.release()
                recv.acquire()
                if done: break
                value = (yield value)
        finally:
            done = True
            send.release()
    thread = threading.Thread(target=threadF, daemon=True)
    thread.start()

    return iterator()


def run_prolog(pvars, F, frame):
    def gg(cb):
        def pcb(frame, F):
            Frw = F.rename_vars(lambda v: frame.get(v, v))
            df = {k:frame.get(k, None) for k in pvars} if pvars else frame
            cb((df, Frw))
            return frame, F
        prolog_driver(F, pcb).refine_and_execute(frame)
    return callback_to_iterator(gg)


########################################################################################################################
# for debugging

class TableBuilder(FBaseType):
    def __init__(self, args, ret, retd, F):
        self.args = args
        self.ret = ret
        self.retd = retd
        self.F = F
    @property
    def children(self):
        return (self.F,)
    @property
    def vars(self):
        return (self.ret, *self.args)
    def run(self, frame):
        frame, res = saturate(frame, self.F)
        if res is done:
            key = tuple(frame.getVariable(v) for v in self.args)
            v = frame.getVariable(self.ret)
            if v is not None:
                self.retd.setdefault(key, []).append(v)
            return frame, done
        return frame, (self if res is self.F else TableBuilder(self.args, self.ret, self.retd, res))
    def rewrite(self, rewriter=lambda x:x):
        return TableBuilder(self.args, self.ret, self.retd, rewriter(self.F))
    def rename_vars(self, remap):
        return TableBuilder(tuple(map(remap, self.args)), remap(self.ret), self.retd, self.F.rename_vars(remap))

def make_table(args, ret, F, frame={}):
    retd = {}
    builder = TableBuilder(args, ret, retd, F)
    builder = builder.refine(frame)
    loop(args, builder, lambda a,b: None, bind_all=True)
    return retd

########################################################################################################################
# sample methods

@recursive_callable
def fmth(ret, X, Y, Z):
    # f(X, Y, Z) = X+Y.
    #ret = localVar('ret')
    K = localVar('K')
    return add(ret, X, Y)



@recursive_callable
def lmth(ret, X, Y):
    # l(X, Y) += I for I:X..Y.
    I = localVar('I')
    expr = range_v(I, X, Y)

    return aggregator_outer(ret, loop((X, Y), (I,), aggregator_inner(I, operator.add, expr)))


@recursive_callable
def umethod(ret, X, Y):
    # f(X, Y) += X*Z for Z in range(1,Y), Y > 5, Y in range(1,25).
    # f(X, Y) += X*Z2 for Z2 in range(1,Y), Y in range(1,25).

    Z = localVar('Z')
    Z2 = localVar('Z2')

    res1 = localVar('res1')
    c1 = localVar('c1')
    c5 = localVar('c5')
    c25 = localVar('c25')
    const = intersect(constant(c1, 1), constant(c5, 5), constant(c25, 25))

    res2 = localVar('res2')

    f1 = intersect(mul(res1, X, Z), range_v(Z, c1, Y), range_v(Y, c1, c25), gt(Y, c5))
    f2 = intersect(mul(res2, X, Z2), range_v(Z2, c1, Y), range_v(Y, c1, c25))

    return intersect(const,
                     aggregator_outer(ret, (X, Y), operator.add,
                                      union((X,Y),
                                            aggregator_inner(res1, (Z,), f1),
                                            aggregator_inner(res2, (Z2,), f2),
                                      )))

    # return intersect(const,  # we have to evaluate the consant constraints
    #                  aggregator_outer(ret,
    #                                   union((X, Y),
    #                                         loop((X, Y), (res1, Z),  aggregator_inner(res1, operator.add, f1)),
    #                                         loop((X, Y), (res2, Z2), aggregator_inner(res2, operator.add, f2))
    #                                   )))



@recursive_callable
def defaultsEx(ret, X, Y):
    # f(X,Y) += X.  % nothing for Y
    # f(X,Y) += X*Y for Y in range(1,25).

    res2 = localVar('res2')

    c1 = localVar('c1')
    c25 = localVar('c25')

    const = intersect(constant(c1, 1), constant(c25, 25))

    return intersect(const,
                     aggregator_outer(ret,
                                      union((X, Y),
                                            aggregator_inner(X, operator.add, done),  # done indicates no constraints
                                            aggregator_inner(res2, operator.add, mul(res2, X, Y))
                                      )))


@recursive_callable
def depedTypes(ret, X, Y):
    # depending on the value of Y, we might or might not have a strategy to loop over X
    # which essentially gives us dependant types
    # f(X, Y) += 1 for X in range(1,10).
    # f(X, Y) += 2 for Y > 5.

    c1 = localVar('c1')
    c2 = localVar('c2')
    c5 = localVar('c5')
    c10 = localVar('c10')

    const = intersect(constant(c1, 1), constant(c2, 2), constant(c5, 5), constant(c10, 10))

    f1 = range_v(X, c1, c10)
    f2 = gt(Y, c5)

    return intersect(const,
                     aggregator_outer(ret, (X, Y), operator.add,
                                      union((X, Y),
                                            aggregator_inner(c1, (), f1),
                                            aggregator_inner(c2, (), f2)
                                      )))

    # return intersect(const,
    #                  aggregator_outer(ret,
    #                                   union((X, Y),
    #                                         # no loop as we are not introducing new variables that we need to loop over
    #                                         aggregator_inner(c1, operator.add, f1),
    #                                         aggregator_inner(c2, operator.add, f2),
    #                                   )))



@recursive_callable
def list_len(length, X):
    # don't use an aggrgator, as we are trying to handle the :- case and be able to both
    # generate a list of some length and compute what the length should be of a given list

    c1 = localVar('c1')
    const = constant(c1, 1)
    len2 = localVar('tmp2')  # == length - 1
    Xs = localVar('Xs')
    Xhead = localVar('Xhead')

    return intersect(const,
                     union((length, X),
                           intersect(constant(length, 0), quote('nil', X)),
                           intersect(add(length, len2, c1), quote('.', X, Xhead, Xs), gteq(length, c1), list_len(len2, Xs))
                     ))


# doesn't work in backwards chaining mode, even with this thing..sigh, I suppose that is expected
# @recursive_callable
# def geo(ret):
#     # a += 1.
#     # a += a/2.

#     c1 = localVar('c1')
#     c2 = localVar('c2')
#     const = intersect(constant(c1, 1), constant(c2, 2))

#     res2 = localVar('res2')
#     geo_res = localVar('aret')

#     return intersect(const,
#                      aggregator_outer(ret,
#                                       union((),
#                                             aggregator_inner(c1, operator.add, done),
#                                             aggregator_inner(res2, operator.add, intersect(geo(geo_res), mul(geo_res, c2, res2)))
#                                       )))



########################################################################################################################


@recursive_callable
def deleteone(Z, lst, RR):
    # the Z is returned with an approperate iterator over elements of this list
    # basically as a linked list of union iterators....but should still be ok
    X = localVar('X')
    Xs = localVar('Xs')
    Ys = localVar('Ys')
    return union((Z, lst, RR),
                 quote('.', lst, Z, RR),  # find the element to delete, no variables so no loop
                 (#loop((Z, lst, RR), (X, Xs, Ys),
                      intersect(quote('.', lst, X, Xs), quote('.', RR, X, Ys), deleteone(Z, Xs, Ys) ))
    )

@recursive_callable
def permutation(A, B):
    Y = localVar('Y')
    Ys = localVar('Ys')
    PYs = localVar('PYs')
    return union((A, B),
                 intersect(quote('nil', A), quote('nil', B)),  # empty list case
                 (#loop((A, B), (Y, Ys, PYs),
                      intersect(quote('.', B, Y, PYs), deleteone(Y, A, Ys), permutation(Ys, PYs),
                      )),
    )


# this is what I would expect dyna ASTs to generate, as they would decorate things with aggregators
# as they still require consolidation

@recursive_callable
def deleteone_withagg(trueRet, Z, lst, RR):
    X = localVar('X')
    Xs = localVar('Xs')
    Ys = localVar('Ys')
    deleteR = localVar('deteleRet')
    ctrue = localVar('ctrue')
    const = constant(ctrue, True)

    return intersect(const,
                     aggregator_outer(trueRet, (Z, lst, RR), operator.or_,
                                      union((Z, lst, RR),
                                            aggregator_inner(ctrue, (), quote('.', lst, Z, RR)),
                                            aggregator_inner(ctrue, (X, Xs, Ys),
                                                             intersect(quote('.', lst, X, Xs),
                                                                       quote('.', RR, X, Ys),
                                                                       deleteone_withagg(deleteR, Z, Xs, Ys)
                                                                  )
                                                             )))
    )


    # return intersect(const,
    #                  aggregator_outer(trueRet,
    #                                   union((Z, lst, RR),
    #                                         aggregator_inner(ctrue, operator.or_, quote('.', lst, Z, RR)),  # find the element to delete, no variables so no loop

    #                                         aggregator_inner(ctrue, operator.or_,
    #                                                          loop((Z, lst, RR), (X, Xs, Ys),
    #                                                               intersect(quote('.', lst, X, Xs),
    #                                                                         quote('.', RR, X, Ys),
    #                                                                         deleteone_withagg(deleteR, Z, Xs, Ys)
    #                                                               )
    #                                                          ))
    #                                   )))

@recursive_callable
def permutation_withagg(trueRet, A, B):
    Y = localVar('Y')
    Ys = localVar('Ys')
    PYs = localVar('PYs')
    ctrue = localVar('ctrue')
    const = constant(ctrue, True)

    deleteR = localVar('deleteRet'),
    recurseR = localVar('permutationRet')

    return intersect(const,
                     aggregator_outer(trueRet, (A, B), operator.or_,
                                      union((A, B),
                                            aggregator_inner(ctrue, (), intersect(quote('nil', A), quote('nil', B))),  # empty list case
                                            aggregator_inner(ctrue, (Y, Ys, PYs),
                                                             intersect(quote('.', B, Y, PYs),
                                                                       deleteone_withagg(deleteR, Y, A, Ys),
                                                                       permutation_withagg(recurseR, Ys, PYs),
                                                                       constant(deleteR, True), constant(recurseR, True)  # check return true
                                                             )))))

    # return intersect(const,
    #                  aggregator_outer(trueRet,
    #                                   union((A, B),
    #                                         aggregator_inner(ctrue, operator.or_, intersect(quote('nil', A), quote('nil', B))),  # empty list case
    #                                         aggregator_inner(ctrue, operator.or_,
    #                                                          loop((A, B), (Y, Ys, PYs),
    #                                                                intersect(quote('.', B, Y, PYs),
    #                                                                          deleteone_withagg(deleteR, Y, A, Ys),
    #                                                                          permutation_withagg(recurseR, Ys, PYs),
    #                                                                          constant(deleteR, True), constant(recurseR, True)  # check return true
    #                                                                )))
    #                                   )))


def mklist(*a):
    if not a:
        return ('nil',)
    h, *s = a
    return ('.', h, mklist(*s))

########################################
# some constraint programming style expressions where a list contains all distinct elements
# not in list will create a non equals constraint between all elements in the list

# we /can/ check these, but they are not unrolled until at least they have one ground argument (for indientification of unique states)
# so in the case of not_in_list, if we had the list [1,2,3,X,Y,Z,4,5,6], then the head of the list would still
# be represented as a free variable where the tail of the list [4,5,6] has been constructed but due to the middle
# there are delayed quote constraints for building the rest of the list.  We are then not going to have any more information
# than the list is just a /free/ variable and so this just sticks around as a delayed constraint to check
#
# in the case of list length, having `list_length(5, -X)` the 5 is a ground value which is can perform induction on to see
# that it is not hitting the same state as an earlier stage, so it would unroll it all of the way.
#
# something could /learn/ that it wants to unroll these methods a few number of (bounded) steps.  That could be performed
# during an optional optimization pass.  At which point any constraints that are gathered from these expressions
# would be things that we can combine and reuse elsewhere in the program.
#
# essentially this is evidence that the rule in recursive_callable is conservative.  Though we would also need to eleminate
# the unions in these rules somehow? to actually be able to make use of these constraints otherwise the disjunction
# prevents us from making use of anything
#
# note, this limitation would also prevent us from determining what the length of a list is that contains any non-ground elements

@recursive_callable
def not_in_list(X, lst):
    # check that the element X is not in the lst
    Y = localVar('Y')
    Ys = localVar('Ys')
    return union((X, lst),
                 quote('nil', lst),  # then the list is empty
                 intersect(quote('.', lst, Y, Ys), not_equal(Y, X), not_in_list(X, Ys))
    )

@recursive_callable
def distinct(lst):
    # all elements of the list are distinct
    X = localVar('X')
    Xs = localVar('Xs')

    return union((lst),
                 quote('nil', lst),  # list is empty
                 intersect(quote('.', lst, X, Xs), not_in_list(X, Xs), distinct(Xs))
    )

@recursive_callable
def sorted_list(lst):
    A = localVar('A')
    As = localVar('As')
    X = localVar('X')
    Xs = localVar('Xs')
    Y = localVar('Y')
    Ys = localVar('Ys')

    return union((lst,),
                 quote('nil', lst),  # empty list is sorted
                 intersect(quote('.', lst, A, As), quote('nil', As)),  # single element in the list, so sorted
                 intersect(quote('.', lst, X, Xs), quote('.', Xs, Y, Ys), lteq(X, Y), sorted_list(Xs))
    )


@recursive_callable
def even_len_list(lst):
    X = localVar('X')
    Xs = localVar('Xs')
    Y = localVar('Y')
    Ys = localVar('Ys')

    return union((lst),
                 quote('nil', '.'),
                 intersect(quote('.', lst, X, Xs), quote('.', Xs, Y, Ys), even_len_list(Ys))
    )

@recursive_callable
def odd_len_list(lst):
    X = localVar('X')
    Xs = localVar('Xs')
    return intersect(quote('.', lst, X, Xs), even_len_list(Xs))


########################################################################################################################
# mapl neural network example


@recursive_callable
def weight(ret, X):
    # weight(0) = 0
    # weight(1) = 1
    # weight(-1) = -1


    # cn1 = localVar('cn1')
    # c0 = localVar('c0')
    # c1 = localVar('c1')
    # consts = intersect(constant(cn1, -1), constant(c0, 0), constant(c1, 1))

    # return intersect(consts,
    #                  aggregator_outer(ret,
    #                                   union((X,),
    #                                         aggregator_inner(c1, operator.add, constant(X, 1)),
    #                                         aggregator_inner(c0, operator.add, constant(X, 0)),
    #                                         aggregator_inner(cn1, operator.add, constant(X, -1)),
    #                                   )))

    val_map = {
        0: 0,
        1: 1,
        -1: -1
    }
    lookup = moded_op('weight_memotable', {
        (False, False): lambda a,b: (val_map.keys(), None),
        (True, False): lambda a,b: (a, val_map[a]) if a in val_map else failure
    })
    return lookup(X, ret)



@recursive_callable
def edge(ret, inp, out):
    cpy = lambda x:x  # just the = aggregator, need better definition mechnism for aggregators.....
    X = localVar('X')
    Dx = localVar('Dx')
    sXDx = localVar('sum_x_dx')
    weight_ret = localVar('weight_ret')
    return aggregator_outer(ret, (inp, out), cpy,
                            aggregator_inner(weight_ret, (X, Dx, sXDx, weight_ret), intersect(quote('inp', inp, X), quote('out', out, sXDx), add(sXDx, X, Dx),
                                                                       weight(weight_ret, Dx))))

@recursive_callable
def neural_input(ret, X):
    # have to make this formed like the input
    w = localVar('w')
    return intersect(weight(ret, w), quote('inp', X, w)) # reuse weight as I am lazy...

@recursive_callable
def neural_output(ret, X):
    # out(X) += inp(Y) * edge(X, Y).
    Y = localVar('Y')
    inpR = localVar('input_ret')
    edgeR = localVar('edge_ret')
    mulR = localVar('mul_ret')

    return aggregator_outer(ret, (X,), operator.add,
                            aggregator_inner(mulR, (Y,),
                                             intersect(neural_input(inpR, Y), edge(edgeR, Y, X), mul(mulR, inpR, edgeR))))




########################################################################################################################



def main():
    prog1 = add('X', 'Y', 'Z')  # X+Y=Z

    prog2 = intersect(add('X', 'Y', 'Z'), lt('X', '20const'), constant('20const', 20))  # X+Y=Z, X<20


    # prog = union(
    #     intersect(add('X', 'Y', 'Z1'), lt('X', '20const'), constant('20const', 20)),
    #     intersect(add('X', 'YY', 'Z2'), mul('Y', '2const', 'YY'), constant('2const', 2), gt('X', '50const'), constant('50const', 50))
    # )


    prog3 = fmth('Y', 'X', 'X', 'Y')

    #print(saturate(Frame({'X': 22, }), prog))

    # query for umethod(4, X), return the results in a table
    pprint(make_table((2,3), 1, umethod(1,2,3), Frame({2:4})))


    # the X variable on umethod has no method to iterate, so we are going to provide one via intersection
    # note the rules that are active change
    # R=umethod(X, Y), X in range(2,7)
    q2 = intersect(umethod('R','X','Y'), range_v('X', 'const2', 'const7'), constant('const2', 2), constant('const7', 7))

    pprint(make_table(('X','Y'), 'R', q2, Frame()))


    print('we are eleminating the branch that we can not loop over, so an iterator\n'
          'is returned in the X space for values that can be iterated over')
    pprint(saturate(Frame({'Y':2}), depedTypes('R','X','Y')))

    print('in this case both branches are kept around, so no iterator is returned and we can not bring the\n'
          'variable X to ground using this rule')
    pprint(saturate(Frame({'Y':10}), depedTypes('R','X','Y')))

    print('neural network example')
    pprint(make_table(('b',), 'r', neural_output('r', 'b'), Frame() ) )


    lst = mklist(1,2,3)
    print('permutation(+,-)')
    pprint(make_table(('A', 'B'), 'B', permutation('A', 'B'), Frame({'A': lst})))

    print('permutation(-,+)')
    pprint(make_table(('A', 'B'), 'A', permutation('A', 'B'), Frame({'B': lst})))


if __name__ == '__main__':
    main()
