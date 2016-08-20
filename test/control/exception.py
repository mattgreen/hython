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

# Testing that finally is run when return is used
def test_finally_with_return():
    try:
        print("Before return")
        return "return"

    finally:
        print("Finally")

print(test_finally_with_return())

# Test that finally is run when break is used
def test_finally_with_break():
    while True:
        try:
            print("Before break")
            break

        finally:
            print("Finally")

print(test_finally_with_break())

# Test that finally is run when continue is used
def test_finally_with_continue():
    i = 0

    while i == 0:
        try:
            print("Before continue")
            i = i + 1
            continue

        finally:
            print("Finally")

print(test_finally_with_continue())

# Do we handle exceptions raised in the exception handler?
def test_raise_in_exception_handler():
    try:
        print("Before first raise")
        raise Exception("oops")
    except:
        print("Handling")
        raise Exception("again")

try:
    test_raise_in_exception_handler()
except:
    print("Second exception caught")

# Do we handle exceptions raised in the finally block?
def test_raise_in_finally():
    try:
        print("Before first raise")
        raise Exception("oops")
    finally:
        print("Handling")
        raise Exception("again")

try:
    test_raise_in_finally()
except:
    print("Second exception caught")

# Exception handling should unwind to the try
def test_unwind():
    a = 42
    print(a)

    raise Exception("goodbye!")

a = 14
try:
    test_unwind()
except:
    print(a)

# Exceptions raised within exception handlers require that the finally block be run
def test_finally_block_run_when_raising_in_exception_handler():
    try:
        print("Before first raise")
        raise Exception("oops")
    except:
        print("Except")
        raise Exception("Not again!")
    finally:
        print("Finally")

try:
    test_finally_block_run_when_raising_in_exception_handler()
except:
    print("Second exception caught")

# Exceptions raised within finally blocks should be handled properly
def test_finally_block_raises_exception():
    try:
        print("Before raise")
        raise Exception("oops")
    except:
        print("Caught")
        raise Exception("Uh oh!")
    finally:
        print("Finally")
        raise Exception("ARGHHH")
        print("Shouldn't see me")

try:
    print("test_finally_block_raises_exception()")
    test_finally_block_raises_exception()
except:
    print("All done")

# Handler with same class should match
try:
    raise Exception("test")

except Exception:
    print("Caught")

# Handler with base class should match
try:
    raise Exception("test")

except BaseException:
    print("Caught")

# Should skip incorrect handlers
try:
    raise Exception("test")
except TypeError:
    print("TypeError")
except NameError:
    print("NameError")
except Exception:
    print("Exception")
except BaseException:
    print("BaseException")

# Should assign the exception to the name given by the handler
try:
    raise Exception("test")
except Exception as e:
    print(e)

# Test that the correct handler is located, even if it exists in outer scopes
def test_caught_by_outside_handlers():
    try:
        raise Exception("hmm")
    except TypeError:
        print("TypeError")

try:
    test_caught_by_outside_handlers()

except Exception:
    print("Caught")

# Test that re-raising exceptions works as it should
def test_reraise():
    try:
        raise Exception("hmm")

    except Exception:
        print("Inside catch")
        raise

try:
    test_reraise()

except Exception:
    print("Caught again")

def test_reraise_catchall():
    try:
        raise Exception("hmm")

    except:
        print("Inside catch")
        raise

try:
    test_reraise_catchall()

except Exception:
    print("Caught again")

# Test break contained in try
try:
    while True:
        print("About to break")
        break
finally:
    print("OK!")


# Test continue contained in try
try:
    i = 0
    while i < 3:
        i += 1
        print("Loop")
        if i == 2:
            print("2!")
            continue
finally:
    print("OK!")

# Test return with while in try
def test_return_in_while():
    try:
        while True:
            return 42
    finally:
        print("Inner finally")

try:
    print(test_return_in_while())
finally:
    print("OK!")

# Test that only objects can be raised
try:
    raise 3
except TypeError as e:
    print("TypeError1")

# Test that only classes derived from BaseException can be raised
class T:
    def __str__(self):
        return "T"

try:
    raise T("e")
except TypeError:
    print("TypeError2")

# Test that only classes derived from BaseException can be caught
try:
    try:
        raise Exception("test")
    except T:
        pass
except TypeError:
    print("TypeError3")
except:
    print("caught something else")
