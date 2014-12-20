a = 5
print("hi")

while a != 0:
    print(a)
    a = a-1
    break
    print("after")
print("outside")


def f(b):
    while b != 0:
        print(b)
        b = b - 1

        #if b == 1:
            #return None
        #else:
        if b == 2:
            break

a = 2
while a != 0:
    f(3)
    a = a - 1
