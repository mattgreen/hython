class C:
    def __init__(self):
        print("__init__")
    def m(self):
        return 42
    def n(self, count):
        return count + 1

c = C()
print(c.m())
print(c.n(42))
