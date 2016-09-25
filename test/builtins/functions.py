# len
print(len("hello"))
print(len(()))
print(len((1,2)))
print(len([]))
print(len([1]))
print(len([1,2,]))

# abs, min, max
# TODO: min, max for iterables or >2 parameters
numbers = [-99, -25.5, -25.5, -0.0, 0, 3/2, 1000000000]
for a in numbers:
    print("abs() =", a)
    for b in numbers:
        print(min(a,b))
        print(max(a,b))

# TODO: more built-in functions
