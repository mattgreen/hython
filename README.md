#hython

A Python 3 interpreter implemented in Haskell.

The goal of this project is to become more familiar with Haskell. It is not to produce a replacement for CPython or PyPy. I would like to implement as much of the core language as I can. While I list features out below, they're more for me to remember what I have remaining in each part of the project.

## Features

### Lexical Analysis

 * [x] Line comments
 * [x] Significant indentation
 * [x] Integer literals
 * [x] Floating point literals
 * [x] Imaginary literals
 * [x] Single quoted string literals
 * [x] Double quoted string literals
 * [ ] Triple quoted string literals
 * [ ] String escape sequences
 * [ ] Byte literals
 * [x] All operators, delimiters, and keywords
 * [ ] Implicit line joining in collections
 * [ ] Explicit line joining via `\\`

### Parsing

 * [x] Scalar literals
 * [x] Adjacent string literal concatenation
 * [x] Function calls
 * [x] Attribute access
 * [x] Tuples
 * [x] List literals
 * [ ] List comprehensions
 * [ ] Dictionary literals
 * [ ] Subscript operators
 * [ ] Set literals
 * [x] All arithmetic operators
 * [x] All bitwise operators
 * [x] All boolean operators
 * [ ] All comparison operators
 * [x] `assert` statement
 * [x] `break` statement
 * [x] `class` statement
 * [x] `def` statement
 * [x] `del` statement
 * [x] `for` statement
 * [x] `global` statement
 * [x] `if`/`elif`/`else` statements
 * [ ] `import` statement (and friends)
 * [x] `lambda` statement
 * [x] `nonlocal` statement
 * [x] `pass` statement
 * [x] `raise` statement
 * [x] `return` statement
 * [x] `try` statement
 * [x] `while` statement
 * [x] `with` statement
 * [ ] `yield` statement
 * [ ] Keyword arguments to functions
 * [ ] Default arguments to functions

### Interpreter

 * [x] `print` built-in function
 * [x] Variable assignment and retrieval
 * [x] Loop control flow
 * [x] Function control flow
 * [x] Scoping (basic)
 * [x] Functions
 * [x] Classes
 * [ ] Inheritance
 * [ ] Modules
 * [ ] Exception handling
 * [ ] Generators
 * [ ] Standard built-in functions
 * [ ] Built-in exception types

## Running

To build:

    $ make

To test:

    $ make test

## Reference
 * [Python 3.4 Language Reference](https://docs.python.org/3.4/reference/)
 * [Alex + Happy Whitespace Handling](https://github.com/jmoy/alexhappy)
 * [berp](https://github.com/bjpop/berp)
 * [Language.Python](https://github.com/bjpop/language-python)
