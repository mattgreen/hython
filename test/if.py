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

if True:
    if 1:
        print("Nested If")

print("After")
