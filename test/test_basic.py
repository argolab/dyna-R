from dyna import *

def test_most_basic():
    # X+Y
    rexpr, ret = M.add(1,2)

    frame = interpreter.Frame()
    frame[1] = 7
    frame[2] = 8
    rr = interpreter.simplify(rexpr, frame)

    assert rr == interpreter.Terminal(1)
    assert ret.getValue(frame) == 15


def test_two_add():
    # (X + Y) + Z

    rexpr1, ret1 = M.add(1,2)
    rexpr2, ret2 = M.add(ret1, 3)

    rexpr = Intersect(rexpr1, rexpr2)

    frame = Frame()
    frame[1] = 7
    frame[2] = 8
    frame[3] = 9
    rr = simplify(rexpr, frame)

    assert rr == interpreter.Terminal(1)
    assert ret2.getValue(frame) == 24


def test_simple_range():
    rexpr, ret = M.range(1,2,3)

    r = {ret: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    frame[1] = 4  # 1 < 4 < 7
    frame[2] = 1
    frame[3] = 7

    rr = simplify(rexpr, frame)
    assert rr == Terminal(1)


def test_sum_aggregator():
    # f(X, Y) += R for R:X..Y.
    v1,v2,v3,v4 = variables_named(1,2,3,4)

    rexpr, ret1 = M.range(v1,v2,v3)
    rexpr = Aggregator(v4, (v2,v3), v1, AggregatorOpImpl(lambda a,b: a+b), rexpr)

    r = {ret1: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    v2.setValue(frame, 1)
    v3.setValue(frame, 4)

    rr = simplify(rexpr, frame)

    assert rr == Terminal(1)
    assert v4.getValue(frame) == 6


def test_basic_union():
    # f(X) += R for R:1..X.
    # f(X) += R for R:X..20.

    a1, r1, lret = variables_named(1, 2, 3)

    rexpr1, ret1 = M.range(lret, constant(1), a1)
    rexpr2, ret2 = M.range(lret, a1, constant(20))

    rpart = Partition((a1, lret), (rexpr1, rexpr2))

    rexpr = Aggregator(r1, (a1,), lret, AggregatorOpImpl(lambda x,y: x+y), rpart)
    r = {ret1: constant(True), ret2: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    a1.setValue(frame, 5)

    rr = simplify(rexpr, frame)

    assert rr == Terminal(1)

    assert r1.getValue(frame) == 190


def test_overlap_union():
    # f(X) += R for R:1..X*2.
    # f(X) += R for R:X..20.

    a1, r1, lret = variables_named(1, 2, 3)

    rexpr_mul, a2mul = M.mul(constant(2), a1)
    rexpr1, ret1 = M.range(lret, constant(1), a2mul)
    rexpr1 = Intersect(rexpr1, rexpr_mul)
    rexpr2, ret2 = M.range(lret, a1, constant(20))

    rpart = Partition((a1, lret), (rexpr1, rexpr2))

    rexpr = Aggregator(r1, (a1,), lret, AggregatorOpImpl(lambda x,y: x+y), rpart)
    r = {ret1: constant(True), ret2: constant(True)}
    rexpr = rexpr.rename_vars(lambda x: r.get(x,x))

    frame = Frame()
    a1.setValue(frame, 5)

    rr = simplify(rexpr, frame)

    assert rr == Terminal(1)

    assert r1.getValue(frame) == 225


def test_basic_term():
    # a = &x(1,2), a=&x(X, Y).

    a,x,y = variables_named(1,2,3)

    rexpr = BuildStructure('x', constant(Term('x', (1, 2))), (x, y))

    frame = Frame()
    rr = simplify(rexpr, frame)

    assert rr == Terminal(1)
    assert x.getValue(frame) == 1
    assert y.getValue(frame) == 2


def test_basic_method_call():
    add_call = context.dyna_system.call_term('add', 2)

    frame = Frame()
    frame[0] = 1
    frame[1] = 2

    rr = saturate(add_call, frame)
    assert rr == Terminal(1)
    assert interpreter.ret_variable.getValue(frame) == 3


def test_list_length():
    list_len = context.dyna_system.call_term('list_length', 2)

    lst = Term.fromlist([1,2,3,4])

    frame = Frame()
    frame[1] = lst

    rr = saturate(list_len, frame)
    assert rr == Terminal(1)
    assert interpreter.ret_variable.getValue(frame) == True
    assert frame[0] == 4


from dyna.interpreter import VariableId
from dyna.context import dyna_system

# deleteone([Z|Zs], Zs, Z).
# deleteone([X|Xs], [X|Ys], Z) :- deleteone(Xs, Ys, Z).
deleteone =  Intersect(Unify(constant(True), interpreter.ret_variable),
                       Partition(variables_named(0,1,2),
                                 (BuildStructure('.', VariableId(0), (VariableId(2), VariableId(1))),
                                  Intersect(BuildStructure('.', VariableId(0), (VariableId('X'), VariableId('Xs'))),
                                            BuildStructure('.', VariableId(1), (VariableId('X'), VariableId('Ys'))),
                                            dyna_system.call_term('deleteone', 3)(*variables_named('Xs', 'Ys', 2)))
                                 )))
dyna_system.define_term('deleteone', 3, deleteone)

# permutation([], []).
# permutation(A, [Z|Rs]) :- deleteone(A, R, Z), permutation(R, Rs).
permutation = Intersect(Unify(constant(True), interpreter.ret_variable),
                        Partition(variables_named(0,1),
                                  (Intersect(BuildStructure('nil', VariableId(0), ()), BuildStructure('nil', VariableId(1), ())),
                                   Intersect(BuildStructure('.', VariableId(1), variables_named('Z', 'Rs')),
                                             dyna_system.call_term('deleteone', 3)(*variables_named(0, 'R', 'Z')),
                                             dyna_system.call_term('permutation', 2)(*variables_named('R', 'Rs'))
                                ))))
dyna_system.define_term('permutation', 2, permutation)

def test_deleteone():
    deleteone_call = dyna_system.call_term('deleteone', 3)

    frame = Frame()
    frame[0] = Term.fromlist([1,2,3,4])
    frame[2] = 3

    rr = saturate(deleteone_call, frame)
    assert rr == Terminal(1)
    assert frame[1].aslist() == [1,2,4]

def test_deleteone2():
    deleteone_call = dyna_system.call_term('deleteone', 3)

    frame = Frame()
    frame[0] = Term.fromlist([3,4,3])
    frame[2] = 3

    rr = saturate(deleteone_call, frame)

    cnt = 0
    def cntr(a,b):
        nonlocal cnt
        cnt += 1
    loop(rr, frame, cntr)
    assert cnt == 2  # check that the callback was done twice, but didn't check that it got the two distinct values...

def test_deleteone3():
    deleteone_call = dyna_system.call_term('deleteone', 3)

    frame = Frame()
    frame[0] = Term.fromlist([1,2,3,4])

    rr = saturate(deleteone_call, frame)

    cnt = 0
    def cntr(a,b):
        nonlocal cnt
        cnt += 1
    loop(rr, frame, cntr)
    assert cnt == 4  # check that the callback was done twice, but didn't check that it got the two distinct values...


def test_permutation():
    # this needs to loop over the different branches that can be constructed

    permute_call = dyna_system.call_term('permutation', 2)

    lst = Term.fromlist([1,2,3,4])

    frame = Frame()
    frame[0] = lst

    rr = saturate(permute_call, frame)

    # v = rr.children[0].arguments[0]
    # v2 = {v: VariableId('foo')}
    # r2 = rr.rename_vars(lambda x: v2.get(x,x))

    cnt = 0
    def cntr(r, f):
        nonlocal cnt
        cnt += 1
    loop(rr, frame, cntr, till_terminal=True)

    assert cnt == 24

from dyna.builtins import gteq, lteq, sub, add

rrv = variables_named('RR')[0]
fib = Aggregator(interpreter.ret_variable, variables_named(0), rrv, AggregatorOpImpl(lambda a,b: a+b),
Partition(variables_named(0, rrv),
                (Intersect(Unify(constant(0), VariableId(0)), Unify(constant(0), rrv)),  # fib(0) = 0
                 Intersect(Unify(constant(1), VariableId(0)), Unify(constant(1), rrv)),  # fib(1) = 1
                 Intersect(gteq(VariableId(0), constant(2)), lteq(VariableId(0), constant(40)),  # fib(X) = X >= 2, X <= 40, fib(X-1) + fib(X-2).
                           sub(VariableId(0), constant(1), ret=VariableId('Xm1')),
                           sub(VariableId(0), constant(2), ret=VariableId('Xm2')),
                           dyna_system.call_term('fib', 1)(VariableId('Xm1'), ret=VariableId('F1')),
                           dyna_system.call_term('fib', 1)(VariableId('Xm2'), ret=VariableId('F2')),
                           add(VariableId('F1'), VariableId('F2'), ret=rrv)
                 )))
)
dyna_system.define_term('fib', 1, fib)

def test_fib_basic():
    dyna_system.delete_term('fib', 1)
    dyna_system.define_term('fib', 1, fib)

    fib_call = dyna_system.call_term('fib', 1)

    frame = Frame()
    frame[0] = 4

    rr = saturate(fib_call, frame)

    assert rr == Terminal(1)
    assert interpreter.ret_variable.getValue(frame) == 3


def test_fib_unk_memos():
    dyna_system.delete_term('fib', 1)

    fibm = rewrite_to_memoize(fib, is_null_memo=False)

    dyna_system.define_term('fib', 1, fibm)

    dyna_system.run_agenda()

    fib_call = dyna_system.call_term('fib', 1)

    # use a large number to ensure that we are doing stuff with memos
    frame = Frame()
    frame[0] = 30  # because this does the computation on the stack, we end up hitting the recursion depth error if we go too deep

    rr = saturate(fib_call, frame)

    assert rr == Terminal(1)
    assert interpreter.ret_variable.getValue(frame) == 832040



def test_fib_null_memos():
    dyna_system.delete_term('fib', 1)

    fibm = rewrite_to_memoize(fib, is_null_memo=True)

    dyna_system.define_term('fib', 1, fibm)

    dyna_system.run_agenda()

    # going to lookup directly into the memo table to determien what the values are
    mt = fibm.body.memos.memos
    assert len(mt._children) == 41
    assert mt._children[(40, 102334155)] == [Terminal(1)]




def test_reflect():
    res, name, nargs, alist = variables_named(*'abcd')

    rf = ReflectStructure(res, name, nargs, alist)

    frame = Frame()
    name.setValue(frame, 'test')
    nargs.setValue(frame, 3)

    rr = simplify(rf, frame)

    assert isinstance(rr._children[0], BuildStructure)
    assert rr._children[0].name == 'test'
    assert len(rr._children[0].arguments) == 3


    frame2 = Frame()
    res.setValue(frame2, Term('test', (1,2,3)))

    rr = simplify(rf, frame2)

    assert alist.isBound(frame2)

    assert alist.getValue(frame2) == Term.fromlist([1,2,3])
    assert name.getValue(frame2) == 'test'
    assert nargs.getValue(frame2) == 3


def test_evaluate():
    ret, name, nargs, alist = variables_named(*'abcd')

    e = Evaluate(dyna_system, ret, name, nargs, alist)

    dyna_system.define_term('test_foo', 3, Unify(VariableId(0), VariableId(1)))

    frame = Frame()
    name.setValue(frame, 'test_foo')
    nargs.setValue(frame, 3)

    rr = simplify(e, frame)

    # this should be the same unify as the body of test_foo
    assert isinstance(rr._children[0], Unify)

    frame = Frame()
    name.setValue(frame, 'test_no')
    nargs.setValue(frame, 3)

    rr = simplify(e, frame)

    assert rr == Terminal(0)


def test_merge_rules():
    z, rv = variables_named(0, 'RR')

    agg_op = AggregatorOpImpl(lambda a,b: a+b)


    r1 = Aggregator(interpreter.ret_variable, (z,), rv, agg_op,
                    Partition((z, rv),
                              [Unify(z, rv)]))

    dyna_system.add_to_term('merge_rule', 1, r1)

    r2 = Aggregator(interpreter.ret_variable, (z,), rv, agg_op,
                    Partition((z, rv),
                              [add(constant(1), z, ret=rv)]))

    dyna_system.add_to_term('merge_rule', 1, r2)

    mc = dyna_system.call_term('merge_rule', 1)
    frame = Frame()
    z.setValue(frame, 3)

    rr = simplify(mc, frame)

    assert interpreter.ret_variable.getValue(frame) == 7


def test_optimizer1():
    fibo = run_optimizer(fib, variables_named(0,interpreter.ret_variable))

def test_optimizer2():
    # check that we can perform inference on the types of a tuple and use that
    # to perform the approperate reflection and then inline calls.  This should
    # also eleminate excess variables from the expression.

    dyna_system.define_term('opt_call', 2, Intersect(Unify(constant(True), interpreter.ret_variable), Unify(*variables_named(0,1))))  # opt_call(X,X).

    res, sv, a1, a2, sname, snargs, alist = variables_named(*'abcdefg')

    # res = *&opt_call(A1, A2)
    rx = Intersect(BuildStructure('opt_call', sv, (a1, a2)), ReflectStructure(sv, sname, snargs, alist), Evaluate(dyna_system, res, sname, snargs, alist))

    rr = run_optimizer(rx, (a1, a2, res))

    assert set(rr._children) == set((Unify(constant(True), res), Unify(a1,a2)))


def test_optimizer3():

    dyna_system.define_term('opt_call', 2, Intersect(Unify(constant(True), interpreter.ret_variable), Unify(*variables_named(0,1))))  # opt_call(X,X).

    res, sv, a1, a2, sname, snargs, alist = variables_named(*'abcdefg')

    # res = *&opt_call(A1, 7).
    rx = Intersect(BuildStructure('opt_call', sv, (a1, constant(7))), ReflectStructure(sv, sname, snargs, alist), Evaluate(dyna_system, res, sname, snargs, alist))

    rr = run_optimizer(rx, (a1, a2, res))

    # there should just be two unify expressions with constants
    assert set(rr._children) == set((Unify(constant(True), res), Unify(a1,constant(7))))


def test_optimizer4():
    # the occurs check performed by the optimizer

    a, b = variables_named(*'ab')

    # X = s(s(X))
    rx = Intersect(BuildStructure('s', a, (b,)), BuildStructure('s', b, (a,)))

    rr = run_optimizer(rx, (a,b))

    assert rr == Terminal(0)
