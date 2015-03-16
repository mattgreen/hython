class EnterAndExit(object):
    def __init__(self, v=False):
        self.exit_value = v

    def __enter__(self):
        print("__enter__")
        return self

    def __exit__(self, cls, e, tb):
        if cls is not None:
            print(cls.__name__)
        if e is not None:
            print(e.__str__())
        if tb is not None:
            print(tb.__class__.__name__)

        print("__exit__")

        return self.exit_value

print("Normal case, no as")
with EnterAndExit():
    print("Block")

print("Normal case, as")
with EnterAndExit() as o:
    print("Block")

print("Exceptional case re-raising")
try:
    with EnterAndExit():
        raise BaseException("hi")
except:
    print("Caught")

print("Exceptional case suppressed by context manager")
with EnterAndExit(True):
    raise Exception("suppressed")

print("Nested, non-exceptional case")
with EnterAndExit(), EnterAndExit():
    print("yep")

print("Nested, exceptional case")
with EnterAndExit(True) as a, EnterAndExit() as b:
    print(a.__class__.__name__)
    raise Exception("suppressed")
