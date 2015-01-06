simple = lambda x: x
print(simple(42))

abc = 42
adds_enclosing = lambda x: x + abc
print(adds_enclosing(18))

uses_other_lambdas = lambda x, y: simple(x) + adds_enclosing(y)
print(uses_other_lambdas(2, 99))

def returns_lambda():
    x = 12
    return lambda a: a + x

print(returns_lambda()(77))
