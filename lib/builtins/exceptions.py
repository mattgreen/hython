class BaseException(object):
    def __init__(self, message=""):
        self._message = message

    def __str__(self):
        return self._message

class SystemExit(BaseException):
    pass

class KeyboardInterrupt(BaseException):
    pass

class GeneratorExit(BaseException):
    pass

class Exception(BaseException):
    pass

class StopIteration(Exception):
    pass

class ArithmeticError(Exception):
    pass

class AssertionError(Exception):
    pass

class AttributeError(Exception):
    pass

class BufferError(Exception):
    pass

class EOFError(Exception):
    pass

class ImportError(Exception):
    pass

class IndexError(Exception):
    pass

class KeyError(Exception):
    pass

class LookupError(Exception):
    pass

class MemoryError(Exception):
    pass

class NameError(Exception):
    pass

class OSError(Exception):
    pass

class ReferenceError(Exception):
    pass

class RuntimeError(Exception):
    pass

class SyntaxError(Exception):
    pass

class SystemError(Exception):
    pass

class TypeError(Exception):
    pass

class ValueError(Exception):
    pass

class Warning(Exception):
    pass
