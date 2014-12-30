module Anima.Types where

type TermVar = Int
type Env = [Term]

data Nat
    = Z
    | S Nat
  deriving (Show, Eq)

data TermBase
    = Unit
    | Type
    | TUnit
    | ATrue
    | AFalse
    | TBool
    | ANat Nat
    | TNat
  deriving (Show, Eq)

data TermBinder
    = Lam
    | Pi
  deriving (Show, Eq)

data Term
    = Base TermBase
    | Var TermVar
    | Binder TermBinder Term Term
    | Apply Term Term
  deriving (Show, Eq)
