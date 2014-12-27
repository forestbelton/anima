module Anima.Types where

type Var = Int
type Env = [Expr]

data Expr
    = EUnit          -- 1
    | TType          -- Type
    | TUnit          -- Type of 1
    | EVar Var       -- x
    | EAbs Expr Expr -- λ x:T . M
    | EPi Expr Expr  -- ∀ x:T . M
    | EApp Expr Expr -- M N
  deriving (Show, Eq)
