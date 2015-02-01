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

# Test simple instantiation
c = C(42)

# Test adding an arbitrary attr
c.other_attribute = "hello"
print(c.other_attribute + " there")

# Test special __class__ var
print(c.__class__)

# Test simple zero arg method
print(c.m())

# One arg method
print(c.n(42))

# Defined attributes
print(c.num)
print(c.get_num())

class Derived(C):
    def m(self):
        return 84

# Test 'inheritance'
print(Derived(99).m())

d = D(10)
print(isinstance(d, D))
print(isinstance(d, C))
