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

#def test_handlers_cleaned_up():
    #try:
        #return 10
    #except:
        #print("you should never see me")

#try:
    #test_handlers_cleaned_up()
    #raise Exception("test")
#except:
    #print("Should only see me")

