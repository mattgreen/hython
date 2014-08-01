class C:
    def __init__(self, n):
        print("__init__")
        print(n)
        self.num = 8

    def m(self):
        return 42
    def n(self, count):
        return count + 1
    def get_num(self):
        return self.num

class D(C):
    pass

c = C(42)
print(c.__class__)
print(c.m())
print(c.n(42))
print(c.num)
print(c.get_num())
