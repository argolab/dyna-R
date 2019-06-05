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
        key = self._mkfilter(key)
        # because None can match multiple things, this doesn't necessarly every
        # get down to only matching a single item.  So we are going to have to
        # keep around the different possible branches....
        return PrefixTrie(0, _filter=key, _root=self._root)

    def __setitem__(self, key, value):
        key = self._mkfilter(key)
        a = self._root
        for i in key[:-1]:
            a = a.setdefault(i, {})
        a[key[-1]] = value

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
                w = r.get(z)
                if w is not None:
                    yield from r(prefix+(z,), f, w)


        yield from r((), self._filter, self._root)

    def __bool__(self):
        # if we can find something that matches then we are non-empty
        # just going to use the iterator for now I suppose...
        for a in iter(self):
            return True
        return False
