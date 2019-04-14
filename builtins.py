add = moded_op('add', {
    (True, True, True):  lambda a,b,c: (b+c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a-b) ,
    (True, False, True): lambda a,b,c: (a, a-c, c) ,
    (False, True, True): lambda a,b,c: (b+c, b, c) ,
})

sub = lambda a,b,c: add(b,a,c)

mul = moded_op('mul', {
    (True, True, True):  lambda a,b,c: (b*c, b, c) ,
    (True, True, False): lambda a,b,c: (a, b, a/b) if b != 0 else error ,  # use the error state in div by 0
    (True, False, True): lambda a,b,c: (a, a/c, c) if c != 0 else error ,
    (False, True, True): lambda a,b,c: (b*c, b, c) ,
})

div = lambda a,b,c: mul(b,a,c)

range_v = moded_op('range', {
    (False, True, True): lambda a,b,c: (range(b,c), b, c) ,
    (True, True, True):  lambda a,b,c: (range(b,c), b, c) ,
})

abs_v = moded_op('abs', {
    (True,True):  lambda a,b: (abs(b), b) ,
    (False,True): lambda a,b: (abs(b), b) ,
    (True,False): lambda a,b: (a, [a,-a]) if a > 0 else ((a, 0) if a == 0 else error) ,
})

lt = check_op('lt', lambda a,b: a < b)

lteq = check_op('lteq', lambda a,b: a <= b)


# just rewrite in terms of lt so that we can demo the
# rewriting of range constraints into the range constraint
gt = lambda a,b: lt(b,a)
gteq = lambda a,b: lteq(b,a)

int_v = check_op('int', lambda x: isinstance(x, int))
