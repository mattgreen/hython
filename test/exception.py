try:
    print("Hello")
except:
    print("Shouldn't see this")

try:
    raise Exception("hello")
except:
    print("Caught the exception!")

def f():
    raise Exception("uh oh")

    print("Shouldn't see this")

try:
    f()

except:
    print("Caught exception from f()")

