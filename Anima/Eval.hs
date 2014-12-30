module Anima.Eval where

import Anima.Types
import Anima.Substitution

import Control.Applicative

look :: TermVar -> [a] -> a
look _ []     = error "couldn't find var"
look 0 (x:xs) = x
look n (x:xs) = look (n - 1) xs

-- type checking
typeOf :: Env -> Term -> Term
typeOf env (Base Unit)      = Base TUnit
typeOf env (Base ATrue)     = Base TBool
typeOf env (Base AFalse)    = Base TBool
typeOf env (Base (ANat _))  = Base TNat
typeOf env (Base x)         = Base Type -- Type : Type for now
typeOf env (Var v)          = look v env
typeOf env (Binder Lam t m) = Binder Pi t $ typeOf (t:env) m
typeOf env (Binder Pi t m)  = Base Type
typeOf env (Apply a b)      = let ta = typeOf env a
                                  tb = typeOf env b in
    case beta env a of
        Binder Lam s t ->
            if s == tb
                then subst t b
                else error $ "s = " ++ show s ++ ", tb = " ++ show tb
        x ->
            error $ "lhs of apply not a pi type (" ++ show x ++ ")"

-- evaluation
beta :: Env -> Term -> Term
beta env (Base x)       = Base x
beta env (Var v)        = Var v
beta env (Binder b t m) = let env' = beta env t : env in
                            Binder b t (beta env' m)
beta env (Apply a b)    = let ba = beta env a
                              bb = beta env b in
                            case ba of
                                Binder Lam _ m ->
                                    beta env $ subst m bb
                                _ ->
                                    Apply ba bb