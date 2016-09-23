print([])
print([1])
print([1,2])
print([1,2,])
print([1,2,3][0])
print([1,2,3][1])

l = [5,6,7,8]

# negative indexing
print(l[-1])
print(l[-3])

# indexing by booleans
print([1,2,3][False])
print([1,2,3][True])

# index out of bounds
try:
    print(l[4])
except IndexError:
    print("IndexError")
try:
    print(l[-5])
except IndexError:
    print("IndexError")

# print lists
print([-5,4,472])
print([[[]]])


l = list()
print(l.__len__())

l.append(1)
print(l.__len__())

l.append(2)
print(l.__len__())

r = list()
r.append(1)
l.extend(r)
print(l.__len__())

l.clear()
print(l.__len__())

l = [1,5,10]
print(1 in l)
print(8 in l)
print(8 not in l)

# construct list from iterable
l1 = list([3,4,5])
print(len(l1))
print(l1)
l2 = list("test")
print(len(l2))

# iterate over list
for e in l2:
    print(e)

# assignment
l[0] = 99
l[-1] = -1
l[True] = list()
print(l)

# order of evaluation
def p(x):
    print(x)
    return x
p(l)[p(0)] = p(2)
