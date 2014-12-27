module Anima.Eval where

import Anima.Types
import Control.Applicative

look :: Var -> [a] -> Maybe a
look _ []     = Nothing
look 0 (x:xs) = Just x
look n (x:xs) = look (n - 1) xs

-- type checking
typeOf :: Env -> Expr -> Maybe Expr
typeOf env EUnit      = Just TUnit
typeOf env TType      = Just TType -- Type : Type for now
typeOf env TUnit      = Just TType
typeOf env (EVar v)   = look v env
typeOf env (EAbs t m) = EPi t <$> typeOf (t:env) m
typeOf env (EPi t m)  = Just TType
typeOf env (EApp a b) = do
    ta <- typeOf env a
    tb <- typeOf env b

    case ta of
        EPi s t ->
            if s == tb
                then Just $ apply_subst b t
                else Nothing
        _       ->
            Nothing

-- evaluation
shift :: Var -> Var -> Expr -> Expr
shift c d EUnit      = EUnit
shift c d TType      = TType
shift c d TUnit      = TUnit
shift c d (EVar i)   = EVar $ if i < c
    then i
    else i + d
shift c d (EAbs t m) = EAbs t (shift (c + 1) d m)
shift c d (EPi t m)  = EPi t (shift (c + 1) d m)
shift c d (EApp a b) = EApp (shift c d a) (shift c d b)

subst :: Var -> Expr -> Expr -> Expr
subst j s EUnit      = EUnit
subst j s TType      = TType
subst j s TUnit      = TUnit
subst j s (EVar i)   = if i == j
    then s
    else EVar i
subst j s (EAbs t m) = EAbs t (subst (j + 1) (shift 1 0 s) m)
subst j s (EPi t m)  = EPi t (subst (j + 1) (shift 1 0 s) m)
subst j s (EApp t u) = EApp (subst j s t) (subst j s u)

apply_subst :: Expr -> Expr -> Expr
apply_subst a b = shift (-1) 0 (subst 0 (shift 1 0 a) b)

beta :: Expr -> Expr
beta EUnit      = EUnit
beta TType      = TType
beta TUnit      = TUnit
beta (EVar v)   = EVar v
beta (EAbs t e) = EAbs t (beta e)
beta (EPi t e)  = EPi t (beta e)
beta (EApp a b) = beta (apply_subst a b)

-- helper constructors
unit :: Expr
unit = EUnit

eID :: Expr -> Expr
eID ty = EAbs ty (EVar 0)
