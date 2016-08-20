print(not True)
print(not False)
print(not not True)
print(not not False)

for a in [False, True]:
    for b in [False, True]:
        print(a  or  b)
        print(a  and b)
        print(a  or  not b)
        print(a  and not b)
        print(not a  or  b)
        print(not a  and b)
        print(not a  or  not b)
        print(not a  and not b)


# Truthiness
print([] or {})
print("" or 0)
print(1  or "test")
print({False: 1} or [2,3])

print([0] and "")
print("abc" and [1])
print(0 and {2: True})
print({} and 1231)


# Test short-circuiting behavior
def t(n):
    print("t", n)
    return True

def f(n):
    print("f", n)
    return False

for a in [t, f]:
    for b in [t, f]:
        for c in [t, f]:
            print(a(1) and b(2) and c(3))
            print(a(1) and b(2) or  c(3))
            print(a(1) or  b(2) and c(3))
            print(a(1) or  b(2) or  c(3))

