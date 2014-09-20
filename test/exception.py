try:
    print("Hello")
except:
    print("Shouldn't see this")

try:
    print("Hello")
except:
    print("Shouldn't see this")
else:
    print("Else")

try:
    raise Exception("hello")
except:
    print("Caught the exception!")

try:
    print("Try")
finally:
    print("Finally")

try:
    raise Exception("Hello")
except:
    print("Except")
finally:
    print("Finally")

def f():
    raise Exception("uh oh")

    print("Shouldn't see this")

try:
    f()

except:
    print("Caught exception from f()")

