import operator  # used for the aggregator ops
import inspect   # used for assert statements only
from pprint import pprint, pformat  # for demo in main

from collections import defaultdict
from textwrap import dedent
import itertools
from functools import reduce

from typing import *

import cython

slower_checks = False


########################################
# Frame

@cython.final
@cython.cclass
class ConstantVariable:
    value : object
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

@cython.cclass
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

@cython.cclass
class _EmptyFrame(Frame):
    def __setitem__(self, var, val):
        assert False  # don't set on this frame directly, can return a new instance that will
    # this is an empty dict, so bool(self) == False
    def update(self, *args, **kwargs):
        assert False

emptyFrame = _EmptyFrame()

@cython.cclass
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

##################################################

class InterpreterContext:
    # Track which operations are performed when evaluating F so that we can
    # generate a trace and use that to compile the F structure into something
    # that can run /efficiently/.  Going to want to track which operation is
    # /called/ next so that those can be transformed into some form of a jump
    # operation?
    #
    # if we are generating cython, then building the jump table would probably
    # be somewhat difficult, so need to think a bit more about what the
    # interpreter expression is going to look like.
    #
    # during tracing this should generate some code which is able to handle the
    # moded case that we have encountered.  But this is going to potentially
    # have some complications with ending up in new states?
    pass


##################################################


@cython.cclass
class FBaseType:
    _hashcache : int
    def __init__(self):
        self._hashcache = 0
    def __call__(self, frame:Frame=emptyFrame) -> Tuple[Frame,'FBaseType']:
        # note that always calling with an empty frame, this is going to have to do rewrites
        # going to have to do rewrites to store values long term

        return self.run(frame)
    def run(self, frame:Frame) -> Tuple[Frame,'FBaseType']:
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
        hv = self._hashcache
        if hv != 0:  # this would just keep getting recomputed...
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
    def __init__(self, func:Callable[[Frame],Tuple[Frame,FBaseType]]):
        super().__init__()
        self.func = func
    def run(self, frame:Frame):
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




def my_hash(obj):
    # make set, dict, list and general iterables hashable
    try:
        return hash(obj)
    except TypeError:
        if isinstance(obj, dict):
            obj = obj.items()
        r = hash(type(obj))
        for v in obj:
            r ^= my_hash(v)  # order is not going to matter as this is just xor
        return r

@cython.cclass
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

@cython.final
@cython.cclass
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
@cython.final
@cython.cclass
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

@cython.final
@cython.cclass
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

@cython.final
@cython.cclass
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


########################################################################################################################
# Main included operations
#
# this includes intersection and union (comma and semicolon) as well as loop which will bring variables down to ground values
# so that in dyna we can aggregate them
#
# The aggregator is split into two parts which differentate between where we compute the final result and the computation
# that takes place inside of the loop on each iteration that add the result to the aggregators internal state

@cython.final
@cython.cclass
class Intersect(FBaseType):
    def __init__(self, a :FBaseType, b :FBaseType):
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

@cython.final
@cython.cclass
class Union(FBaseType):
    def __init__(self, unioned_vars :tuple, a :FBaseType, b :FBaseType, switching_var_name=None):
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

    assert False  # TODO: rewrite to handle renaming the variables in children.
                  # This is going to make the above implementation simpler, and
                  # a frame object easier to pass around hopefully

    return Union(unioned_vars, a, b)


class UnboundLoopedVariablesError(RuntimeError):
    def __init__(self, vars, *args):
        super().__init__(*args)
        self.vars = vars


def loop(loops :Tuple, frame :Frame, F :FBaseType, callback :Callable[[Frame], Tuple[Frame, FBaseType]], *, bind_all: bool=False):
    # loop represents a strategy for binding variables that are defined in the F
    # we are going to collect iterables from the frame and use those to drive the loop, we are going to recurse
    # calling ourselves on nested versions
    #
    # we are going to bind at least one variable, which corresponds with making progress.  We can bind more than one variable if
    # we can find something that does that
    #
    # callback(frame, F).  Returned value is ignored

    frame, res = F(frame)
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

def build_index_over_variables(frame :Frame, F :FBaseType, vars :Tuple):
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
    def __init__(self, name, ops, ops_strs, vars):
        self.vars_ = vars
        self.ops = ops
        self.ops_strs = ops_strs  # for compiling, we are going to require the string representation of the python code
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
        return ModedOp(self.name, self.ops, self.ops_strs, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def _get_iterators(self, ret):
        for var, val in zip(self.vars, self.execute(emptyFrame)):
            if hasattr(val, '__iter__'):
                ret[var].add(IteratorFromIterable(var, val))
    def cython_string(self):
        ret = f"""
        result = ({self.ops_strs[...]})({','.join(map(..., self.vars))})
        if result == error:
            goto error state
        """
        for v in self.vars:
            if this is bound:
                # check the equality between the expressions
                pass
            else:
                # then we are going to assign a value to the variable
                pass

        ret += f"""
        # need to check that the currently assigned variables are equal with the returned result
        # then we need to assign the resulting variables
        ({','.join(map(..., self.vars))} ,) = result
        """

def moded_op(name, op):
    arity = max(map(len, op.keys()))
    assert arity == min(map(len, op.keys()))
    op_e = {k: eval(v, globals()) for k,v in op.items()}
    def method_locations(*locs):
        assert len(locs) == arity
        return ModedOp(name, op_e, op, locs)
    return method_locations


class CheckOp(FBaseType):
    def __init__(self, name, op, op_str, vars):
        self.vars_ = vars
        self.name = name
        self.op = op
        self.op_str = op_str
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
        return CheckOp(self.name, self.op, self.op_str, tuple(map(remap, self.vars)))
    def possibly_equal(self, other):
        return type(self) is type(other) and self.op is other.op
    def cython_string(self):
        return f"""
        result = ({self.op_str})({','.join(map(..., self.vars))})
        if result == done:
            goto done state
        else:
            goto failure state
        """


# check only works in the fully ground case, so we don't care about any other modes atm
def check_op(name, op):
    if slower_checks:
        arity = len(inspect.getfullargspec(op).args)
    op_e = eval(op, globals())
    def method_locations(*locs):
        if slower_checks:
            assert len(locs) == arity
        return CheckOp(name, op_e, op, locs)
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
# everything above this line should deal with some internal representation about the backchaining interpreter



class OptionalFailFast(FBaseType):
    # these are for infered constraints that we are allowed to not check, and for
    def __init__(self, wrapped):
        self.wrapped = wrapped
    @property
    def children(self):
        return (self.wrapped,)
    def run(self, frame):
        # randomly choose if it wants to run it?
        frame, res = self.wrapped(frame)
        if res is self.wrapped:
            return frame, self
        if res is done or res is failure:
            return frame, res
        if res is error:
            return frame, done
        return frame, OptionalFailFast(res)
    def disp(self, indent):
        return f'opt({self.wrapped.disp(indent+"    ")})'
    def _get_iterators(self, ret):
        # in the case of getting iterators, we are already allowed to pick between these
        # so we are just going to always include these objects
        self.wrapped._get_iterators(ret)


class Speculate(FBaseType):
    def __init__(self, wrapped :FBaseType):
        self.wrapped = wrapped
    def children(self):
        return (self.wrapped,)
    def run(self, frame):
        # we are not going to run this expression directly.  We are going to
        # look for a memoized expression where we can lookup a given expression
        return frame, self
    def disp(self, indent):
        return f'speculate({self.wrapped.disp(indent+" "*10)})'
    def _get_iterators(self, ret):
        # we do not get iterators unless
        pass

class SpeculateLookup(FBaseType):
    def __init__(self, wrapped :FBaseType):
        self.wrapped = wrapped
    def children(self):
        return (self.wrapped,)
    def run(self, frame):
        pass
    def disp(self, indent):
        return f'lookup(....)'
    def _get_iterators(self, ret):
        # this needs to return the iterators over the internal stored memoization table
        pass


########################################################################################################################




# the "returned" value is the first argument
# because we are representing the expressions as being attached to variables, there is not "proper" return variable
# instead I am just using this as a convention in this file
add = moded_op('add', {
    (True, True, True):  'lambda a,b,c: (b+c, b, c)' ,
    (True, True, False): 'lambda a,b,c: (a, b, a-b)' ,
    (True, False, True): 'lambda a,b,c: (a, a-c, c)' ,
    (False, True, True): 'lambda a,b,c: (b+c, b, c)' ,
})

sub = lambda a,b,c: add(b,a,c)

mul = moded_op('mul', {
    (True, True, True):  'lambda a,b,c: (b*c, b, c)' ,
    (True, True, False): 'lambda a,b,c: (a, b, a/b) if b != 0 else error' ,  # use the error state in div by 0
    (True, False, True): 'lambda a,b,c: (a, a/c, c) if c != 0 else error' ,
    (False, True, True): 'lambda a,b,c: (b*c, b, c)' ,
})

div = lambda a,b,c: mul(b,a,c)

range_v = moded_op('range', {
    (False, True, True): 'lambda a,b,c: (range(b,c), b, c)' ,
    (True, True, True):  'lambda a,b,c: (range(b,c), b, c)' ,
})

abs_v = moded_op('abs', {
    (True,True):  'lambda a,b: (abs(b), b)' ,
    (False,True): 'lambda a,b: (abs(b), b)' ,
    (True,False): 'lambda a,b: (a, [a,-a]) if a > 0 else ((a, 0) if a == 0 else error)' ,
})

lt = check_op('lt', 'lambda a,b: a < b')

lteq = check_op('lteq', 'lambda a,b: a <= b')


# just rewrite in terms of lt so that we can demo the
# rewriting of range constraints into the range constraint
gt = lambda a,b: lt(b,a)
gteq = lambda a,b: lteq(b,a)

int_v = check_op('int', 'lambda x: isinstance(x, int)')


########################################################################################################################


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
