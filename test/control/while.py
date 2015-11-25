i = 5
while i != 0:
    print(i)
    i = i - 1

i = 8
while i != 0:
    print(i)
    i = i - 1

    if i < 2:
        continue
    print("skip")

i = 10
while i != 0:
    print(i)
    j = i
    while j != 0:
        print(j)
        j = j - 1

        if j == 2:
            break
        elif j == 5:
            continue

    i = i - 1

n = 5
while n != 0:
    print(n)
    n = n - 1
else:
    print("what the...")
