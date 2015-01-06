a = 42

def f():
    global a
    print(a)
    a = 43

f()
print(a)
