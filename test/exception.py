# Non-exceptional case of try/except
try:
    print("Hello")
except:
    print("Shouldn't see this")

# Non-exceptional case of try/except/else
try:
    print("Hello")
except:
    print("Shouldn't see this")
else:
    print("Else")

# Exceptional case of try/except
try:
    raise Exception("hello")
except:
    print("Caught the exception!")

# Non-exceptional case of try/finally
try:
    print("Try")
finally:
    print("Finally")

# Exceptional case of try/except/finally
try:
    raise Exception("Hello")
except:
    print("Except")
finally:
    print("Finally")

# Try/except handling exception in called function
def f():
    raise Exception("uh oh")

    print("Shouldn't see this")

try:
    f()

except:
    print("Caught exception from f()")

# Try/finally with exception raised in called function
def g():
    try:
        print("Begin")
        raise Exception("boom")
    finally:
        print("End")

try:
    g()
except:
    print("Caught")
finally:
    print("Cleaned up!")

def h():
    try:
        print("Begin")
        raise Exception("boom")
    except:
        print("Safe!")
    finally:
        print("End")

try:
    h()
except:
    print("Not caught here")
else:
    print("this is run")
finally:
    print("Cleaned up!")

# Testing that return statement peels off exception handlers
def test_handlers_removed_after_return():
    try:
        return 10
    except:
        print("you should never see me")

try:
    print(test_handlers_removed_after_return())
    raise Exception("test")
except:
    print("Should only see me")

# Testing that break in an exception handler removes handlers
def test_handlers_removed_after_break():
    while True:
        try:
            break
        except:
            print("you should never see me")

try:
    print(test_handlers_removed_after_break())
    raise Exception("test")
except:
    print("Should only see me")

# Testing that continue in an exception handler removes handlers
def test_handlers_removed_after_continue():
    i = 0
    while i == 0:
        try:
            i = i + 1
            continue
        except:
            print("you should never see me")

try:
    print(test_handlers_removed_after_continue())
    raise Exception("test")
except:
    print("Should only see me")

