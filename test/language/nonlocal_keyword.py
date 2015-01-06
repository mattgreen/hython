def f():
    x = 0
    print(x)

    def g():
        nonlocal x
        x = 1

    g()
    print(x)

    return x

f()
