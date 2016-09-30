# Boolean operations
print(42 == 42)
print(42 != 18)

# Integer boolean operations
print(5 > 1)
print(5 >= 5)
print(4 < 1)
print(4 <= 4)

# Float boolean operators
print(5.0 > 1.0)
print(4.0 < 1.0)
print(5.0 >= 5.0)
print(4.0 <= 4.0)

# Mixed int/float with operators
print(5 == 1.0)
print(4 != 1.0)
print(5 > 1.0)
print(4 < 1.0)
print(5 >= 5.0)
print(4 <= 4.0)

print(5.0 == 1)
print(4.0 != 1)
print(5.0 > 1)
print(4.0 < 1)
print(5.0 >= 5)
print(4.0 <= 4)


# On strings
print("test" == "test")
print("" == "test")
print("a" < "b")
print("b" < "a")
print("abc" < "abcde")


# On bytes
print(b'test' == b'test')
print(b'' == b'test')
print(b'a' < b'b')
print(b'b' < b'a')
print(b'abc' < b'abcde')


# Mixed
print("mixed!")
elements = [-3, -0.00001, 0, 0.0, 1, 1.0, 32, "", "foo", b'fo', b'foo', False, True, None]
for a in elements:
    for b in elements:
        try:
            print("comparison")
            print(a == b)
            print(a != b)
            print(a < b)
            print(a > b)
            print(a <= b)
            print(a >= b)
        except TypeError:
            print("TypeError")


# Chaining comparison operators
print("chained")
print(3 == 3 != 3)
print(4 == 4 > 2)
print(1 < 2 < 3 < 4)
print(2 > 3 > -1)
print(3 <= 10 > 17)
print(-3 < 10 >= 10)

# Mixed with other operators
print(3 < 2 * 2 < 5)
print(4 // 2 == 2 < 3)
print(7 > 6 > 7 - 1 and (True and False) < True)

def p(n):
    print(n)
    return n

# Only evaluate each element once
print(p(1) < p(2) < p(3) < p(4))

# Stop after first wrong comparison
print(p(7) < p(6) < p(8) < p(9))


# TODO: Comparison of lists, tuples
# TODO: Equality of sets, dicts
# TODO: Imaginary
# TODO: custom objects with __eq__() etc.
