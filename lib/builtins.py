from exceptions import *

class NotImplementedType(object):
    pass

NotImplemented = NotImplementedType()

def isinstance(o, cls):
    return issubclass(o.__class__, cls)

def issubclass(cls, clsInfo):
    return __hython_primitive__("issubclass", cls, clsInfo)

def print(s):
    __hython_primitive__("print", s)

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
