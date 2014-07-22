#hython

A Python 3 interpreter written in Haskell. I'm new to Haskell, so this will move pretty slowly.

I wanted to learn Haskell, so I intentionally chose a more ambitious project to see how it does.

## Features

* Line comments
* `print` function
* Basic data types: integers, strings, and booleans
* Operators: `+`, `-`, `*`, `/`, `==`, `!=`
* Mutable variables
* If/elif/else statements
* While/break
* Functions + `return` statement

## Differences from Python 3

* Wrong indenting is silently ignored, rather than producing an exception

## Running
    $ cabal install
    $ make
    $ ./hython test/fib.py
