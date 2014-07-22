print("Before")
if 1:
    print("In if")
if True:
    print("True")
if 42:
    print("42")
if 0:
    print("Shouldn't see this: 0")
if False:
    print("Shouldn't see this: False")
if None:
    print("Shouldn't see this: None")
else:
    print("Else")

if True:
    if 1:
        print("Nested If")


a = 5
if a == 7:
    print("Shouldn't see this: 7")
elif a == 6:
    print("Shouldn't see this: 6")
elif a == 5:
    print("a is 5")
else:
    print("Shouldn't see this: else")

print("After")
