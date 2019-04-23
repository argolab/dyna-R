# the core R structure such as intersect and partition


class Intersect(RBaseType):

    def __init__(self, children :Tuple[RBaseType]):
        super().__init__()

        self._children = tuple(children)

    @property
    def children(self):
        return self._children

def intersect(children):
    mul = 1
    r = []
    for c in children:
        if isinstance(c, Terminal):
            mul *= c.multiplicity
        else:
            r.append(c)
    if not r or mul == 0:
        return Terminal(mul)
    if mul != 1:
        r.append(Terminal(mul))
    if len(r) == 1:
        return r[0]
    return Intersect(tuple(r))



@simplify.define(Intersect)
def simplify_intersect(self :Intersect, frame: Frame):
    return intersect([simplify(c) for c in self.children])


class Partition(RBaseType):
    """
    This class is /verhy/ overloaded in that we are going to be using the same representation for memoized entries as well as the partitions
    """
    def __init__(self, unioned_vars :Tuple, children :List[RBaseType]):
        super().__init__()
        self.unioned_vars = unioned_vars
        # the children should be considered immutable once placed on the partition class
        # though we are going to construct this class via
        self.children = {(None,)*len(unioned_vars) : children}
    @property
    def vars(self):
        return self.unioned_vars
    @property
    def children(self):
        for v in self.children.values():
            yield from v
    def rewrite(self, rewriter):
        nc = {}
        for k,v in self.children.items():
            nc[k] = tuple(rewriter(w) for w in v)
        return Partition(self.unioned_vars, nc)


# these are now conceptually not written on the class
@simplify.add(Partition)
def simplify_partition(self :Partition, frame: Frame):
    var_vals = tuple(frame.getVariable(u) for u in self.unioned_vars)
    def merge_tuples(a, b):
        for i,j in zip(a,b):
            if i!=j: raise 123  # something that indicates that these are not equal
            yield i or j  # return the one that is not null

    nc = defaultdict(list)
    for k,v in self.children.items():
        # this needs to check that the assignment of variables is consistent otherwise skip it
        # then this needs to figure out what
