# Dyna-R

The Dyna programming language built on R-exprs.

Paper about the internals of this system can be found [here](http://cs.jhu.edu/~mfl/#Evaluation%20of%20Logic%20Programs%20with%20Built-Ins%20and%20Aggregation%3A%20A%20Calculus%20for%20Bag%20Relations).

## Install Instructions
```
git clone git@github.com:matthewfl/dyna-R.git
cd dyna-R
python -m venv /tmp/dyna
source /tmp/dyna/bin/activate
pip install -r requirements.txt
python setup.py develop

# run tests
pytest

# start repl
dyna   OR   python -m dyna.repl
```


## Example interactive session with dyna
```
./dyna   # start dyna

# define finannaci sequence
fib(X) = fib(X - 1) + fib(X - 2) for X > 1.
fib(0) = 0.
fib(1) = 1.

# set fib to be memoized with an unknown default.
# A value will be compute the first time an entry is required
memoize_unk fib/1

# make a query against fib
fib(100)
```

### Operations are derferred when they would cause non-termination
```
deleteone([X|Xs], Xs, X).
deleteone([X|Xs], [X|Ys], Z) :- deleteone(Xs, Ys, Z).
permute([], []).
permute(As, [Z|Bs]) :- deleteone(As, Rs, Z), permute(Rs, Bs).


# permute works in both modes due to R-exprs
permute([1,2,3], X)

permute(X, [1,2,3])
```

### Dyna-R includes the ability to perform *abstract* reasoning in some cases
```
even([]).
even([X,Y|Z]) :- even(Z).
odd([X|Y]) :- even(Y).
even_odd(X) :- even(X), odd(X).

# run the optimizer on the program to identify that even_odd(X) is empty
# even & odd represent incompatiable recursive "types" where their intersection is empty.
optimize

even_odd(X)
```

# Simplified API
Document for the Python API can be found [here](python_api.md).


# [Here be dragons](https://en.wikipedia.org/wiki/Here_be_dragons)

```
              (__)    )
              (..)   /|\
             (o_o)  / | \
             ___) \/,-|,-\
           //,-/_\ )  '  '
              (//,-'\
              (  ( . \_
           gnv `._\(___`.
                '---' _)/
                     `-'
```

This is an "academic" implementation.  There may be bugs in general, though it is surprisingly robust in a lot of cases.  Aka, [here be dragons](https://en.wikipedia.org/wiki/Here_be_dragons).
