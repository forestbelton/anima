module Anima.Eval where

import Anima.Types
import Control.Applicative

look :: TermVar -> [a] -> Maybe a
look _ []     = Nothing
look 0 (x:xs) = Just x
look n (x:xs) = look (n - 1) xs

-- type checking
typeOf :: Env -> Term -> Maybe Term
typeOf env (Base Unit)      = Just (Base TUnit)
typeOf env (Base x)         = Just (Base Type) -- Type : Type for now
typeOf env (Var v)          = look v env
typeOf env (Binder Lam t m) = Binder Pi t <$> typeOf (t:env) m
typeOf env (Binder Pi t m)  = Just (Base Type)
typeOf env (Apply a b)      = do
    ta <- typeOf env a
    tb <- typeOf env b

    case ta of
        Binder Pi s t ->
            if s == tb
                then Just $ apply_subst b t
                else Nothing
        _ ->
            Nothing

-- evaluation
shift :: TermVar -> TermVar -> Term -> Term
shift c d (Base x)       = Base x
shift c d (Var i)        = Var $ if i < c
    then i
    else i + d
shift c d (Binder b t m) = Binder b t (shift (c + 1) d m)
shift c d (Apply a b)    = Apply (shift c d a) (shift c d b)

subst :: TermVar -> Term -> Term -> Term
subst j s (Base x)       = Base x
subst j s (Var i)        = if i == j
    then s
    else Var i
subst j s (Binder b t m) = Binder b t (subst (j + 1) (shift 1 0 s) m)
subst j s (Apply t u)    = Apply (subst j s t) (subst j s u)

apply_subst :: Term -> Term -> Term
apply_subst a b = shift (-1) 0 (subst 0 (shift 1 0 a) b)

beta :: Term -> Term
beta (Base x) = Base x
beta (Var v)  = Var v
beta (Binder b t m) = Binder b t (beta m)
beta (Apply a b) = undefined
