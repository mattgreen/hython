from exceptions import *

class NoneType(object):
    def __bool__(self):
        return False

    def __str__(self):
        return "None"

class ellipsis(object):
    def __bool__(self):
        return True
    def __str__(self):
        return "Ellipsis"

Ellipsis = ellipsis()

class NotImplementedType(object):
    def __bool__(self):
        return True

    def __str__(self):
        return "NotImplemented"

NotImplemented = NotImplementedType()

def isinstance(o, cls):
    return issubclass(o.__class__, cls)

def issubclass(cls, clsInfo):
    return __hython_primitive__("issubclass", cls, clsInfo)

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

class list(object):
    def __init__(self):
        self._list = __hython_primitive__("list-new")

    def __len__(self):
        return __hython_primitive__("list-length", self._list)

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
