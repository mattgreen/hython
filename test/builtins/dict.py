
blank = {}

d = { "key": "value", "key2": 98123 }

print("key" in d)
print("key2" in d)
print("not-in" in d)
print("not-in" not in d)

try:
    d["asdf"]
except KeyError:
    print("Raised KeyError")

# __getitem__
print(d["key"])

# __setitem__
d["new-item"] = 88
print(d["new-item"])
d["new-item"] = 99
print(d["new-item"])

# __delitem__
del d["new-item"]

# get()
print(d.get("key2"))
print(d.get("not-found"))
print(d.get("not-found", 42))

# setdefault()
print(d.setdefault("new-key", "value"))
print(d["new-key"])

# keys()
print(d.keys().__len__())

# values()
print(d.values().__len__())

# clear()
d.clear()
print(d.__len__())
