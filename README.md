anima [![status](https://secure.travis-ci.org/forestbelton/anima.png)](http://travis-ci.org/forestbelton/anima)
===

A simple dependently-typed programming language

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
> (lam TE E)
(lam TE E) : EPi TUnit TUnit
> (lam TE 0)
(lam TE 0) : EPi TUnit TUnit
>
```