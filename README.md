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

![example](http://i.imgur.com/TydemDi.png)

todo
----

* natural number support
* identity types
* support for strictly positive family definitions
* named variables instead of their de bruijn indices
* type universe hierarchy
* dependent sums
