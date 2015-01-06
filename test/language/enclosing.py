def f():
    a = 10
    def g():
        return a + 1
    return g

print(f()())
