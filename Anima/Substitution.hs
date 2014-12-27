module Anima.Substitution (subst) where

import Anima.Types

-- Shift bindings by d places, with cutoff c
shift :: TermVar -> TermVar -> Term -> Term
shift c d (Base x)       = Base x
shift c d (Var i)        = Var $ if i < c
    then i
    else i + d
shift c d (Binder b t m) = Binder b t (shift (c + 1) d m)
shift c d (Apply a b)    = Apply (shift c d a) (shift c d b)

-- Substitute the jth bound variable with the term s
subst' :: TermVar -> Term -> Term -> Term
subst' j s (Base x)       = Base x
subst' j s (Var i)        = if i == j
    then s
    else Var i
subst' j s (Binder b t m) = Binder b (subst t s) (subst' (j + 1) (shift 1 0 s) m)
subst' j s (Apply t u)    = Apply (subst' j s t) (subst' j s u)

subst :: Term -> Term -> Term
subst a b = shift (-1) 0 (subst' 0 (shift 1 0 a) b)