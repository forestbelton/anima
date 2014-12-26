module Anima.Eval where

import Control.Applicative

type Var = Int
type Env = [Expr]

look :: Var -> Env -> Maybe Expr
look _ []     = Nothing
look 0 (x:xs) = Just x
look n (x:xs) = look (n - 1) xs

data Ty
    = TUnit
    | TFun Ty Ty
  deriving (Eq, Show)

data Expr
    = EUnit          -- 1
    | EVar Var       -- x
    | EAbs Ty Expr   -- λ x:T . M
    | EApp Expr Expr -- M N
  deriving (Show, Eq)

-- type checking
typeOf :: Env -> Expr -> Maybe Ty
typeOf env EUnit      = Just TUnit
typeOf env (EVar v)   = look v env >>= typeOf env
typeOf env (EAbs t m) = TFun t <$> typeOf env m
typeOf env (EApp a b) = do
    ta <- typeOf env a
    tb <- typeOf env b
    case ta of
        TFun t _ ->
            if t == tb
                then Just tb
                else Nothing
        _        -> Nothing

-- evaluation
shift :: Var -> Var -> Expr -> Expr
shift c d EUnit      = EUnit
shift c d (EVar i)   = EVar $ if i < c
    then i
    else i + d
shift c d (EAbs t m) = EAbs t (shift (c + 1) d m)
shift c d (EApp a b) = EApp (shift c d a) (shift c d b)

subst :: Var -> Expr -> Expr -> Expr
subst j s EUnit      = EUnit
subst j s (EVar i)   = if i == j
    then s
    else EVar i
subst j s (EAbs t m) = EAbs t (subst (j + 1) (shift 1 0 s) m)
subst j s (EApp t u) = EApp (subst j s t) (subst j s u)

beta :: Expr -> Expr
beta EUnit      = EUnit
beta (EVar v)   = EVar v
beta (EAbs t e) = EAbs t (beta e)
beta (EApp a b) = shift (-1) 0 (subst 0 (shift 1 0 a) b)

-- helper constructors
unit :: Expr
unit = EUnit

eID :: Ty -> Expr
eID ty = EAbs ty (EVar 0)
