
def method(name, *args):
    # perform lookup on a method given some name.  The arity comes from the number of arguments
    # that are present


    pass

def aggregator(operator, *body):
    op = {
        '+=': lambda x,y: x+y
    }
    pass


def fib(X):
    R = localVar()

    return aggrgator(
        '+=',
        intersect(eq(X, Constant(0)), eq(R, Constant(0))),
        intersect(eq(X, Constant(1)), eq(R, Constant(1))),
        #intersect(gt(X, Constant(1)), add(R, method('fib',
    )
