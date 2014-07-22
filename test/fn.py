def f():
    if True:
        return 0
        print("Shouldn't see this")

    print("Hello")

def g():
    print("Goodbye")

def h():
    return f() + 1

def i():
    pass

print(f())
print(g())
print(h())
print(i())
