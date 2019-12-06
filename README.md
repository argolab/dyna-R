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
