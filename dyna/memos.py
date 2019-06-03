from .interpreter import *

class MemoContainer:

    def __init__(self, supported_mode: Tuple[bool], variables: Tuple[Variable], body: RBaseType):
        assert len(supported_mode) == len(variables)
        self.supported_mode = supported_mode
        self.variables = variables
        self.body = body

        self.memos = {}

    def lookup(self, values):
        assert len(values) == len(self.variables)
        key = []
        for var, imode, val in zip(self.variables, self.supported_mode, values):
            if imode:
                # if it is unbound, then we are going to have to iterate over
                # the domain of some variable, which means that we are going to
                # have to construct the domain of the variable (if we haven't
                # already) and then return a partition which iterates that
                # variable.  We might also want to delay, as constructing the
                # domain of a variable could be an expensive operation that we
                # want to avoid
                assert val is not None and val is not InvalidValue
                key.append(val)
        key = tuple(key)
        if key in self.memos:
            return self.memos[key]
        # then we are going to determine what the result of this memoized value
        # is this requires constructing a new sub interpreter and using that to
        # set the values etc
        frame = Frame()
        for var, imode, val in zip(self.variables, self.supported_mode, values):
            if imode:
                var.setValue(frame, val)
        # determine the new body and frame
        nR = saturate(self.body, frame)
        nR = [nR]
        for var, imode in zip(self.variables, self.supported_mode):
            if not imode and var.isBound(frame):
                # then we need /somewhere/ to store the value of this /ground/ variable
                # so we are just going to add in unification with a constant for now
                # we might also want to instead use the partition system?
                nR.insert(0, Unify(var, constant(var.getValue(frame))))
            var._unset(frame)  # delete this from the frame for the next step

        nR = intersect(*nR)

        # we need to rewrite body such that it doesn't need this frame anymore
        # which means that we are going to remap all of the variables to their constant value
        d = dict((VariableId(k), constant(v)) for k,v in frame.items())
        if d:
            nR = nR.rename_vars(lambda x: d.get(x,x))

        # for now we don't want to deal with cycles where the key might have been set by something else in the processes
        # so we are just going to /assert that away/
        assert key not in self.memos
        self.memos[key] = nR
        return nR


class UnkMemo(RBaseType):

    def __init__(self, variables :Tuple[Variable], memos: MemoContainer):
        assert len(variables) == len(memos.variables)
        self.variables = variables
        self.memos = memos

    @property
    def vars(self):
        return self.variables

    def rename_vars(self, remap):
        return UnkMemo(tuple(remap(v) for v in self.variables), self.memos)

@simplify.define(UnkMemo)
def simplify_unkmemo(self, frame):
    # the idea should be that if we are handling different modes

    mode = tuple(v.isBound(frame) for v in self.variables)
    can_run = True
    for a, b in zip(mode, self.memos.supported_mode):
        if b and not a:
            can_run = False
    if not can_run:
        # then there isn't enough bound that we can attempt to look a memoized
        # value up
        return self
    key = tuple(v.getValue(frame) for v in self.variables)
    res = self.memos.lookup(key)

    # rename the variables and make new spaces for things that were not
    # referenced
    vmap = dict(zip(self.memos.variables, self.variables))
    res2 = res.rename_vars_unique(vmap.get)


    # run the new result once, which can
    return simplify(res2, frame)
