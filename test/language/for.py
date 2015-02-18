a = [1,2,3]
for i in a:
    print(i)

t = (1,2,3)
for i in t:
    print(i)

for c in "hello":
    print(c)

for c in "stuff":
    print("for")
else:
    print("for-else")

for c in "":
    print("for")
else:
    print("for-else")

for c in "stuff":
    print("for")
    break
else:
    print("for-else")

for c in "stuff":
    continue
    print("for")
else:
    print("for-else")
