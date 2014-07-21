def f():
    if True:
        return 0
        print("Shouldn't see this")

    print("Hello")

def g():
    print("Goodbye")

def h():
    return f() + 1

print(f())
print(g())
print(h())
