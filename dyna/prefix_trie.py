_NOT_FOUND = object()
_EMPTY_FILTER = slice(None)

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
        return PrefixTrie(0, _filter=key, _root=self._root)

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

    def __hash__(self):
        return hash(self._root)

    def __eq__(self, other):
        return self is other or \
            (type(self) is type(other) and
             self._filter == other._filter and
             self._root == other._root)