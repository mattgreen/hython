class object:
    def __bool__(self):
        return True

    def __str__(self):
        return "<object>"

class NoneType(object):
    def __bool__(self):
        return False

    def __str__(self):
        return "None"

class ellipsis(object):
    def __str__(self):
        return "Ellipsis"

Ellipsis = ellipsis()

class NotImplementedType(object):
    def __bool__(self):
        return True

    def __str__(self):
        return "NotImplemented"

NotImplemented = NotImplementedType()

#def isinstance(o, cls):
    #return issubclass(o.__class__, cls)

#def issubclass(cls, clsInfo):
    #return __hython_primitive__("issubclass", cls, clsInfo)

def iter(x):
    return basic_iterator(x)

class basic_iterator(object):
    def __init__(self, obj):
        self._obj = obj
        self._index = 0
        self._length = len(obj)

    def __next__(self):
        if self._index >= self._length:
            raise StopIteration()

        o = self._obj[self._index]
        self._index += 1

        return o

def print(*args):
    __hython_primitive__("print", *args)

def str(obj):
    return __hython_primitive__("str", obj)

class dict(object):
    def __init__(self):
        self._dict = __hython_primitive__("dict-new")

    def __bool__(self):
        return self.__len__() > 0

    def __contains__(self, key):
        return __hython_primitive__("dict-contains", self._dict, key)

    def __delitem__(self, key):
        __hython_primitive__("dict-del", self._dict, key)

    def __getitem__(self, key):
        if self.__contains__(key): # not quite optimal
            return __hython_primitive__("dict-get", self._dict, key)
        else:
            raise KeyError("'" + key + "'")

    def __len__(self):
        return __hython_primitive__("dict-length", self._dict)

    def __setitem__(self, key, value):
        __hython_primitive__("dict-set", self._dict, key, value)

    def __str__(self):
        s = "{"

        i = 0
        for key, value in self.items():
            if i > 0:
                s += ", "
            s += str(key) + ": " + str(value)
            i += 1

        s += "}"
        return s

    def clear(self):
        __hython_primitive__("dict-clear", self._dict)

    def get(self, key, default=None):
        if self.__contains__(key):
            return self.__getitem__(key)
        else:
            return default

    def keys(self):
        result = list()

        i = 0
        items = self.items()
        while i < len(items):
            result.append(items[i][0])
            i += 1

        return result

    def items(self):
        return __hython_primitive__("dict-items", self._dict)

    def setdefault(self, key, default=None):
        if self.__contains__(key):
            return self.__getitem__(key)
        else:
            self.__setitem__(key, default)
            return default

    def values(self):
        result = list()

        i = 0
        items = self.items()
        while i < len(items):
            result.append(items[i][1])
            i += 1

        return result


class list(object):
    def __init__(self):
        self._list = __hython_primitive__("list-new")

    def __add__(self, r):
        result = list()
        result.extend(self)
        result.extend(r)
        return result

    def __bool__(self):
        return self.__len__() > 0

    def __contains__(self, item):
        return __hython_primitive__("list-contains", self._list, item)

    def __eq__(self, rhs):
        if len(self) != len(rhs):
            return False

        i = 0
        while i < len(self):
            if self[i] != rhs[i]:
                return False
            i += 1

        return True

    def __getitem__(self, index):
        if index < 0:
            index = self.__len__() - index
        if index >= self.__len__():
            raise IndexError("list index out of range")

        return __hython_primitive__("list-get", self._list, index)

    def __len__(self):
        return __hython_primitive__("list-length", self._list)

    def __mul__(self, n):
        result = list()
        while n > 0:
            result.extend(self)
            n -= 1
        return result

    def __rawitems__(self):
        return self._list

    def __str__(self):
        s = "["

        i = 0
        while i < len(self):
            if i > 0:
                s += ", "
            s += str(self[i])
            i += 1

        s += "]"
        return s

    def append(self, obj):
        __hython_primitive__("list-append", self._list, obj)

    def clear(self):
        __hython_primitive__("list-clear", self._list)

    def extend(self, r):
        if isinstance(r, list):
            __hython_primitive__("list-concat", self._list, r._list)
        else:
            raise TypeError("not iterable")


class range(object):
    def __init__(self, startOrStop, stop=None, step=None):
        if stop is None:
            self.start = 0
            self.stop = startOrStop
        else:
            self.start = startOrStop
            self.stop = stop

        if step is None:
            step = 1

        self.step = 1

        self.values = []

        i = self.start
        while i < self.stop:
            self.values = self.values + [i]
            i = i + self.step

        print(self.values)


class set(object):
    def __init__(self, iterable=[]):
        self._set = {}
        for i in iterable:
            self.add(i)

    def __contains__(self, key):
        return self._set.__contains__(key)

    def __len__(self):
        return self._set.__len__()

    def add(self, o):
        self._set[o] = True

    def clear(self):
        self._set.clear()

    def remove(self, o):
        del self._set[o]

class tuple(object):
    def __init__(self, iterable = None):
        self._items = []
        if iterable:
            self._items.extend(iterable)

    def __add__(self, iterable):
        return tuple(self._items.__add__(iterable))

    def __bool__(self):
        return self._items.__bool__()

    def __contains__(self, obj):
        return self._items.__contains__(obj)

    def __eq__(self, obj):
        return self._items.__eq__(obj)

    def __getitem__(self, i):
        return self._items.__getitem__(i)

    def __hash__(self):
        h = 0
        for i in self:
            h += hash(i)
        return h

    def __mul__(self, iterable):
        return tuple(self._items.__mul__(iterable))

    def __len__(self):
        return self._items.__len__()

    def __rawitems__(self):
        return self._items.__rawitems__()

    def __str__(self):
        s = "("

        i = 0
        while i < self.__len__():
            if i != 0:
                s += ", "
            s += str(self[i])
            i += 1

        if len(self) == 1:
            s += ","
        s += ")"

        return s

class traceback(object):
    pass
