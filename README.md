# Dyna-R

The Dyna programming language built on R-exprs

## install instructions
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

# define fib
fib(X) = fib(X - 1) + fib(X - 2) for X > 1.
fib(0) = 0.
fib(1) = 1.

# set fib to be memoized with an unk default
memoize_unk fib/1

# make a query against fib
fib(100)

```


```
even([]).
even([X,Y|Z]) :- even(Z).
odd([X|Y]) :- even(Y).
even_odd(X) :- even(X), odd(X).

# run the optimizer on the program to identify that even_odd(X) is empty
optimize

even_odd(X)
```


```
deleteone([X|Xs], Xs, X).
deleteone([X|Xs], [X|Ys], Z) :- deleteone(Xs, Ys, Z).
permute([], []).
permute(As, [Z|Bs]) :- deleteone(As, Rs, Z), permute(Rs, Bs).


# permute works in both modes due to R-exprs
permute([1,2,3], X)

permute(X, [1,2,3])
```


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
