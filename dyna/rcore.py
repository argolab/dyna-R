# the core R structure such as intersect and partition

from .interpreter import *


class Intersect(RBaseType):

    def __init__(self, children :Tuple[RBaseType]):
        super().__init__()
        self._children = tuple(children)

    @property
    def children(self):
        return self._children

def intersect(*children):
    mul = 1
    r = []
    for c in children:
        if isinstance(c, Terminal):
            mul *= c.multiplicity
        else:
            r.append(c)
    if not r or mul == 0:
        return terminal(mul)
    if mul != 1:
        r.append(terminal(mul))
    if len(r) == 1:
        return r[0]
    return Intersect(tuple(r))


@simplify.define(Intersect)
def simplify_intersect(self :Intersect, frame: Frame):
    return intersect(*[simplify(c, frame) for c in self.children])


class Partition(RBaseType):
    """
    This class is /verhy/ overloaded in that we are going to be using the same representation for memoized entries as well as the partitions
    """
    def __init__(self, unioned_vars :Tuple, children :Tuple[Tuple[RBaseType, Tuple]]):
        super().__init__()
        self.unioned_vars = unioned_vars
        # the children should be considered immutable once placed on the partition class
        # though we are going to construct this class via

        # make the children simple in that we are just going to scan the list in the case that
        self.children = tuple(children)

    @property
    def vars(self):
        return self.unioned_vars
    @property
    def children(self):
        for v in self.children:
            yield v[0]

def partition(unioned_vars, children):
    # construct a partition
    return Partition(unioned_vars, tuple((c, (None,)*len(unioned_vars)) for c in children))


# these are now conceptually not written on the class
@simplify.define(Partition)
def simplify_partition(self :Partition, frame: Frame):
    var_vals = tuple(u.getValue(frame) for u in self.unioned_vars)
    def merge_tuples(a, b):
        for i,j in zip(a,b):
            if i!=j: raise 123  # something that indicates that these are not equal
            yield i or j  # return the one that is not null

    nc = defaultdict(list)
    assert False
    for k,v in self.children.items():
        # this needs to check that the assignment of variables is consistent otherwise skip it
        # then this needs to figure out what
        pass


@getPartitions.define(Partition)
def getPartitions_partition(self :Partition):
    yield self
    assert False
    # TODO need to determine which variables we can also iterate, so this means
    # looking at the results from all of the children branches and then
    # filtering out things that are not going to work.  if variables are renamed
    # on the different branches, then it is possible that the iterators will
    # have to be able to handle those renamings.

    for p in self.children:
        pass


class Unify(RBaseType):
    def __init__(self, v1, v2):
        self.v1 = v1
        self.v2 = v2
    @property
    def vars(self):
        return (self.v1, self.v2)

@simplify.define(Unify)
def simplify_unify(self, frame):
    if self.v1.isBound(frame):
        v2.setValue(frame, self.v1.getValue(frame))
        return terminal(1)
    elif self.v2.isBound(frame):
        v1.setValue(frame, self.v2.getValue(frame))
        return terminal(1)
    return self
