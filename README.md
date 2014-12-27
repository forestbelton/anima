anima [![status](https://secure.travis-ci.org/forestbelton/anima.png)](http://travis-ci.org/forestbelton/anima)
===

A simple dependently-typed programming language

glossary
--------

The syntax uses parentheses in a way similar to a Lisp.

| Name        | Description                                                                   |
| ----------- | ----------------------------------------------------------------------------- |
| `0, 1, ...` | De Bruijn indices                                                             |
| `E`         | The inhabitant of the unit type                                               |
| `TUnit`     | The type of the unit type (aka TUnit)                                         |
| `Type`      | The type of types (aka TType)                                                 |
| `lam t e`   | A function that accepts an input of type t, and evaluates to the expression e |
| `pi t e`    | The dependent function                                                        |

example
-------

```bash
$ cabal run
Preprocessing executable 'anima' for anima-0.1.0.0...
   __    _  _  ____  __  __    __
  /__\  ( \( )(_  _)(  \/  )  /__\
 /(__)\  )  (  _)(_  )    (  /(__)\
(__)(__)(_)\_)(____)(_/\/\_)(__)(__)  Version 0.1

> E
E : TUnit
> (lam TUnit E)
(lam TUnit E) : EPi TUnit TUnit
> (lam TUnit 0)
(lam TUnit 0) : EPi TUnit TUnit
>
```