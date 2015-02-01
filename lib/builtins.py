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
