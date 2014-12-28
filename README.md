anima [![status](https://secure.travis-ci.org/forestbelton/anima.png)](http://travis-ci.org/forestbelton/anima)
===

A simple dependently-typed programming language

glossary
--------

The syntax uses parentheses in a way similar to a Lisp.

| Name        | Description                                                                   |
| ----------- | ----------------------------------------------------------------------------- |
| `0, 1, ...` | De Bruijn indices                                                             |
| `Unit`      | The inhabitant of the unit type                                               |
| `TUnit`     | The type of the unit type                                                     |
| `Type`      | The type of types (aka TType). Yes, this needs universes                      |
| `lam t e`   | A function that accepts an input of type t, and evaluates to the expression e |
| `pi t e`    | The type of a dependent function                                              |

example
-------

```bash
$ cabal run
Preprocessing executable 'anima' for anima-0.1.0.0...
   __    _  _  ____  __  __    __
  /__\  ( \( )(_  _)(  \/  )  /__\
 /(__)\  )  (  _)(_  )    (  /(__)\
(__)(__)(_)\_)(____)(_/\/\_)(__)(__)  Version 0.1

> Unit
Unit : TUnit
> (lam TUnit Unit)
(lam TUnit Unit) : EPi TUnit TUnit
> (lam TUnit 0)
(lam TUnit 0) : EPi TUnit TUnit
>
```

todo
----

* pretty printing
* natural number support
* identity types
* support for strictly positive family definitions
* named variables instead of their de bruijn indices
* type universe hierarchy
* dependent sums