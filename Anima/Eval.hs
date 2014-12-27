module Anima.Eval where

import Anima.Types
import Anima.Substitution

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

beta :: Term -> Term
beta (Base x) = Base x
beta (Var v)  = Var v
beta (Binder b t m) = Binder b t (beta m)
beta (Apply a b) = undefined
