# This is the documentation for using the simplified Python API

The simplified Python API for Dyna is designed to make common operations *easy*
without requiring too much knowledge about how the system is working internally.


## Example use
```python

from dyna.api import DynaAPI

dyna = DynaAPI()

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

### Defining rules in Dyna

Rules can be defined using `api.add_rules(dyna_code :str)` where any string
passed into this method will be parsed and then loaded into the runtime.

### Wrapper call methods

Using `api.make_call(signature :str)` a wrapper method which will allow calling
directly into the dyna runtime can be constructed.  This can be done using the functor name and arity as `function_name/arity` like `fib/1` above, or
this can be done using `%` as a stand in for arguments, allowing for multiple
methods to be called at the same time.  E.g.  `foo(%, bar(%))` will construct a two argument function which is equivalent to defining `annon_func(A,B) = foo(A, bar(B)).`
and `annon_func/2` being the resulting call.


### Exposing Python functions.

Functions can be exposed from Python using `api.define_function` as a decorator.
E.g.
```python

@api.define_function()
def my_function(Arg1, Arg2):
    return 123

api.add_rules(" result += my_function(A, B) for g(A, B).
```

The decorator takes the optional arguments `name` and `arity`, otherwise it will infer those from the definition of the function.

Note: This function **MUST** be functional and not modify its arguments or have
external side effects.  Doing otherwise will results in bugs as the order in
which functions are invoked or the number of times that a function will be
invoked my change between runs.

### Passing Opaque values through Dyna

The API wrapper supports passing opaque values through the system and getting
them back out (including passing those values into the exposed Python
functions).  This should make it possible to pass around objects such as PyTorch
tensors.

This is done by first creating a reference to a table with a `name` and `arity`
```python
table = api.table('table_name',  2)
```
Then values can be set into the table
```python
table[1,2] = {'some object': 123}
```
This table can be referenced from Dyna code:
```python
api.add_rules('result += table_name(A, B)')
```

There is currently no support for mixing definitions for a table defined this
way and definitions for rules that are defined in Dyna.  However, you can just
proxy between which values
```python
api.add_rules("""
table2(A,B) := table(A, B).  % fallback to the table defined above

table2(1,X) := 7.  % rule defined in Dyna
""")
```


# Example test case

An example test case can be found in [`test/test_python_api.py`](test/test_python_api.py)
