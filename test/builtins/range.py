# creation
r123 = range(4)
rnegative = range(-15, -10)
r4 = range(4, 10, 20)
rempty_1 = range(0)
rempty_2 = range(7, 18, -1)
rreverse = range(20, 10, -1)
r258_1 = range(2, 9, 3)
r258_2 = range(2, 10, 3) # should be identical
r258_3 = range(2, 11, 3) # should be identical

# length
print(len(r123))
print(len(rnegative))
print(len(r4))
print(len(rempty_1))
print(len(rempty_2))
print(len(rreverse))
print("identical:")
print(len(r258_1))
print(len(r258_2))
print(len(r258_3))

# indexing
print(r123[1])
print(r4[0])
print(r4[-1])
print(rnegative[2])
print(rreverse[7])
print(r258_1[-2])
for i in [0,1,2]:
    print(r258_1[i])

# iterating
for e in r123:
    print(e)
for e in rnegative:
    print(e)
for e in r4:
    print(e)
for e in rempty_1:
    print(e)
for e in rempty_2:
    print(e)
for e in rreverse:
    print(e)
for e in r258_1:
    print(e)
for e in r258_2:
    print(e)
for e in r258_3:
    print(e)
# iterate again:
for e in r258_3:
    print(e)

# error handling:
try:
    range(5,10,0)
except ValueError:
    print("step size zero error")
try:
    range(5)[5]
except IndexError:
    print("range index error")
# TODO
# try:
#     range(3.0)
# except TypeError:
#     print("float range error")

# str()
print(r123)
print(rnegative)
print(r4)
print(rempty_1)
print(rempty_2)
print(rreverse)
print(r258_1)

# membership
for e in [2, 8, 13, -13, -20]:
    print(e)
    print(e in r123)
    print(e in r123)
    print(e in rnegative)
    print(e in r4)
    print(e in rempty_1)
    print(e in rempty_2)
    print(e in rreverse)
    print(e in r258_1)

# TODO: equality
# TODO: slicing
