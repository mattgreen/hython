class C:
    def __init__(self, n):
        print("__init__")
        print(n)

    def m(self):
        return 42
    def n(self, count):
        return count + 1

c = C(42)
print(c.__class__)
print(c.m())
print(c.n(42))
