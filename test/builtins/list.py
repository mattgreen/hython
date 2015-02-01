print([])
print([1])
print([1,2])
print([1,2,])
print([1,2,3][0])
print([1,2,3][1])

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
