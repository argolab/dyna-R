_NOT_FOUND = object()
#_EMPTY_FILTER = slice(None)

setdefault = dict.setdefault

class PrefixTrie:
    """
    Basic prefix trie.  None is treated as a wild card.
    """

    __slots__ = ('_root', '_filter')

    def __init__(self, nargs, *, _filter=None, _root=None):
        self._root = _root or {}
        self._filter = _filter or (None,)*nargs

    def _mkfilter(self, key):
        # unsure if this how this filtering should be handled, maybe this should
        # just ignore the new values that are assigned into slots?  Atm with
        # having None, the value of previous filters can impact how many slots
        # it will continue to take which is odd and probably not really
        # something that should be used, rather just updating the tuple with new
        # values that are not null???
        key = tuple(key)
        i = 0
        nk = []
        for f in self._filter:
            if f is None and i < len(key):
                nk.append(key[i])
                i += 1
            else:
                nk.append(f)
        assert i == len(key)  # otherwise there are too many entries on the key for the size of the requested prefix trie
        return tuple(nk)

    def __getitem__(self, key):
        global _NOT_FOUND
        # key = self._mkfilter(key)
        # # because None can match multiple things, this doesn't necessarly every
        # # get down to only matching a single item.  So we are going to have to
        # # keep around the different possible branches....
        # return PrefixTrie(0, _filter=key, _root=self._root)

        r = self.get(key, _NOT_FOUND)
        if r is _NOT_FOUND:
            raise KeyError()
        return r

    def get(self, key, default=None):
        global _NOT_FOUND
        assert len(key) == len(self._filter)
        a = self._root
        for i in key:
            a = a.get(i, _NOT_FOUND)
            if a is _NOT_FOUND:
                return default
        return a

    def __setitem__(self, key, value):
        #key = self._mkfilter(key)
        setdefault = dict.setdefault
        assert len(key) == len(self._filter)
        a = self._root
        for i in key[:-1]:
            a = setdefault(a, i, {})
        a[key[-1]] = value

    def setdefault(self, key, default):
        setdefault = dict.setdefault
        assert len(key) == len(self._filter)
        a = self._root
        for i in key[:-1]:
            a = setdefault(a, i, {})
        return a.setdefault(key[-1], default)

    def filter(self, key):
        # we need to merge this with the operators that are getting filtered
        #assert len(key) == len(self._filter)  # otherwise the number of filtered variables is off and this will return a internal dict
        nfilter = []
        a = b = 0
        while a < len(self._filter):
            if self._filter[a] is None:
                nfilter.append(key[b])
                a += 1
                b += 1
            else:
                nfilter.append(self._filter[a])
                a += 1
        assert b == len(key)

        return PrefixTrie(0, _filter=tuple(nfilter), _root=self._root)

    def delete_all(self):
        # delete everything that matches the current filter
        def r(prefix, f, a):
            z = f[len(prefix)]
            if len(f) == len(prefix) - 1:
                if z is None:
                    a.clear()
                else:
                    assert None not in a  # TODO: how does this get handled, are we deleting it?  It would match I suppose, but it matches many things
                    del a[z]
            else:
                if z is None:
                    for k, v in a.items():
                        r(prefix+(k,), f, v)
                else:
                    if None in a:
                        r(prefix+(None,), f, a[None])
                    w = a.get(z)
                    if w is not None:
                        r(prefix+(z,), f, w)
        r((), self._filter, self._root)

    def __delitem__(self, key):
        # this should maybe
        #setdefault = dict.setdefault
        assert len(key) == len(self._filter)
        a = self._root
        for i in key[:-1]:
            a = a[i]  #setdefault(a, i, {})
        del a[key[-1]]

    def __iter__(self):
        # this iterates over all of the tuples that match the current filter
        def r(prefix, f, a):
            if len(f) == len(prefix):
                yield prefix, a
                return
            z = f[len(prefix)]
            if z is None:
                for k, v in a.items():
                    yield from r(prefix+(k,), f, v)
            else:
                if None in a:
                    yield from r(prefix+(None,), f, a[None])
                w = a.get(z)
                if w is not None:
                    yield from r(prefix+(z,), f, w)
        yield from r((), self._filter, self._root)

    def items(self):
        return iter(self)

    def keys(self):
        for k, _ in self:
            yield k

    def values(self):
        for _, v in self:
            yield v

    def update(self, d):
        # this could be made more efficient I suppose?  Walking multiple
        # branches of the trie at the same time, and sharing structure when possible?
        for k, v in d.items():
            self[k] = v

    def map_values(self, mapper):
        def r(l, a):
            if l == 0:
                return {k: mapper(v) for k,v in a.items()}
            else:
                return {k: r(l-1, v) for k,v in a.items()}
        nr = r(len(self._filter)-1, self._root)
        return PrefixTrie(0, _filter=self._filter, _root=nr)

    def __len__(self):
        # this is not efficient, should have something better here I suppose? if
        # there are filters then this is going to become more of an issue..
        c = 0
        for a in self:
            c += 1
        return c

    def single_item(self):
        a = iter(self)
        try:
            b = next(a)
        except StopIteration:
            return
        try:
            next(a)
        except StopIteration:
            return b

    def __bool__(self):
        # if we can find something that matches then we are non-empty
        # just going to use the iterator for now I suppose...
        for a in iter(self):
            return True
        return False

    # def __hash__(self):
    #     return hash(self._root)

    def __eq__(self, other):
        return self is other or \
            (type(self) is type(other) and
             self._filter == other._filter and
             self._root == other._root)

    def __repr__(self):
        return repr(dict(self))

    @property
    def nvars(self):
        return sum(None is v for v in self._filter)


def zip_tries(Ta, Tb):
    # construct an iterator over both of the elements in the trie with their
    # assocated values.  If one of the tries does not match a particular value,
    # then we are going just return None for that value while still iterating the other trie
    #
    # this should respect the filters of the two tries

    assert len(Ta._filter) == len(Tb._filter)

    fa = Ta._filter
    fb = Tb._filter

    def r(a, b, prefix):
        nonlocal fa, fb
        if a is None and b is None:
            return
        elif len(fa) == len(prefix):
            yield prefix, a, b
        elif a is None:
            bz = fb[len(prefix)]
            if bz is None:
                for k, v in b.items():
                    yield from r(None, v, prefix+(k,))
            else:
                if None in b:
                    yield from r(None, b[None], prefix+(None,))
                w = b.get(bz)
                if w is not None:
                    yield from r(None, w, prefix+(bz,))
            return
        elif b is None:
            az = fa[len(prefix)]
            if az is None:
                for k,v in a.items():
                    yield from r(v, None, prefix+(k,))
            else:
                if None in a:
                    yield from r(a[None], None, prefix+(None,))
                w = a.get(az)
                if w is not None:
                    yield from r(w, None, prefix+(az,))
            return
        else:
            az = fa[len(prefix)]
            bz = fb[len(prefix)]

            if az is None and bz is None:
                for k, v in a.items():
                    yield from r(v, b.get(k), prefix+(k,))
                for k, v in b.items():
                    if k not in a:
                        yield from r(None, v, prefix+(k,))
            elif az == bz:
                if None in a:
                    if None in b:
                        yield from r(a[None], b[None], prefix+(None,))
                    else:
                        yield from r(a[None], None, prefix+(None,))
                elif None in b:
                    yield from r(None, b[None], prefix+(None,))
                yield r(a.get(az), b.get(bz), prefix+(az,))
            else:
                if az is None:
                    yield from r(a.get(bz), b.get(bz), prefix+(bz,))
                    for k, v in a.items():
                        if k != bz:
                            yield from r(v, None, prefix+(k,))

                elif bz is None:
                    yield from r(a.get(az), b.get(az), prefix+(az,))
                    for k,v in b.items():
                        if k != az:
                            yield from r(None, v, prefix+(k,))
                else:
                    yield from r(a.get(az), None, prefix+(az,))
                    yield from r(None, b.get(bz), prefix+(bz,))

                    #assert False  # this will have to handle different filters,
                              # where we are going to get keys from one of the
                              # maps but not the others.  idk if that would
                              # actually be used?

    yield from r(Ta._root, Tb._root, ())
