# hython

A toy Python 3 interpreter implemented in Haskell.

## Introduction

I wanted to learn Haskell, and I wanted a big project, so I decided to write a Python 3 interpreter. The result was extremely educational and easily the coolest project I've ever worked on. Because it's implemented in a naive fashion, it won't ever be a replacement for real Python implementations.

**Note:** Hython only implements most of the Python3 _language_. It doesn't contain much of a standard library, which is a big part of what makes Python pleasant to use. Adding all of the necessary machinery needed for the existing Python 3 standard library to function is an enormous undertaking that I'm not interested in.

## Status

It's finally done! Or at least, I'm declaring it that way.

## Features

 * [x] Lexer
 * [x] Parser
 * [x] Most built-in data types, including `int`, `bool`, `string`, `list`, `dict` and `range`
 * [x] Common unary and binary operators on common data types
 * [x] A few built-in functions, including `print`
 * [x] Variable assignment and lookup, with support for `nonlocal` and `global` keywords
 * [x] Conditional expressions with `if` and `else`
 * [x] All loop constructs: `for` and `while` with support for `break` and `continue` within them
 * [x] Support for the `with` statement
 * [x] Destructuring (`(a,b) = [1,2]`)
 * [x] Functions, including nested functions, default parameters, and keyword parameters
 * [x] Splat (`*` and `**`) operators in caller argument lists
 * [x] Lambda expressions, with proper environment capture
 * [x] Classes, including inheritance and proper method resolution order
 * [x] Objects
 * [x] Exception handling via `try`, with support for handlers, frame unwinding, `finally` handlers, and `else`, along with some built-in exception classes
 * [x] Basic support for loading modules with the `import` statement
 * [x] Simple REPL
 * [x] Support for the `is` operator
 * [ ] Support for generators and `yield`
 * [ ] List/generator/dict/set comprehensions
 * [ ] Index slicing
 * [ ] Support for decorators / metaclasses
 * [ ] Multi-line input for the REPL

## Code Metrics
`sloccount` output as of 10/1/16:

    Totals grouped by language (dominant language first):
    haskell:       2159 (70.83%)
    yacc:           580 (19.03%) # parser
    python:         309 (10.14%) # lib

## Examples

See the [test](https://github.com/mattgreen/hython/tree/master/test) directory for example code that works

## Building and running

1. Install [Stack](https://github.com/commercialhaskell/stack)

2. Clone the repository:

        $ git clone https://github.com/mattgreen/hython.git
        $ cd hython

3. Build:

        $ make

4. Run a file:

        $ ./hython test/fib.py

## REPL

Hython includes a simple REPL, which you can play around with:

    $ ./hython

## Test Suite

Hython's test suite is rather simple: it ensures the output of Hython matches that of the system's `python3` for each test file.

To run the automated test suite:

    $ make test

## Reference Information
 * [Python 3.4 Language Reference](https://docs.python.org/3.4/reference/)
 * [Alex + Happy Whitespace Handling](https://github.com/jmoy/alexhappy)
 * [berp](https://github.com/bjpop/berp)
 * [Language.Python](https://github.com/bjpop/language-python)
