# This is documentation for using the simplified Python API

The simplified Python API for Dyna is designed to make common operations *easy*
wihtout requiring too much knowledge about how the system is working internally.


## Example use
```python

from dyna.api import DynaAPI

dyna = dynaAPI()

# adding rules to the dyna program
dyna.add_rules("""
fib(X) = fib(X-1) + fib(X-2) for X > 1.
fib(0) = 0.
fib(1) = 1.
""")

fib = dyna.make_call('fib/1')     # alternative:   fib = dyna.make_call('fib(%)')

# calls into the system can be easily made
assert fib(10) == 55



```
