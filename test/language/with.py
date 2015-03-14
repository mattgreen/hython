class EnterAndExit(object):
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

print("Normal case, no as")
with EnterAndExit():
    print("Block")

print("Normal case, as")
with EnterAndExit() as o:
    print("Block")

print("Exceptional case, no as")
try:
    with EnterAndExit():
        raise BaseException("hi")
except:
    print("Caught")

try:
    with EnterAndExit() as e:
        raise Exception("hi")
except Exception:
    print("Caught")

