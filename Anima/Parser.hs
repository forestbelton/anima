{-# LANGUAGE NoMonomorphismRestriction #-}

module Anima.Parser where

import Anima.Types
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec

{-

General syntax, a la LISP: (fn arg1)

0, 1, 2, ... represent De Bruijn indices

Base values:
Unit          -- The value of the unit type
TUnit         -- The type of the unit type
Type          -- The type of types

Special forms:

(Lam ty expr) -- s     -> t
(Pi ty expr)  -- (x:t) -> M x
(N expr)      -- (N expr) where N = 0, 1, ...

-}


ws = many (oneOf " \r\t\n")
nt p = string p <* ws

base = (Base Unit <$ nt "Unit")
  <|> try (Base TUnit <$ nt "TUnit")
  <|> try (Base ATrue <$ nt "True")
  <|> (Base AFalse <$ nt "False")
  <|> (Base TBool <$ nt "Bool")
  <|> (Base TNat <$ nt "Nat")
  <|> (Base Type <$ nt "Type")
  <|> (Var . read <$> (many1 (oneOf "0123456789") <* ws))

lambda = Binder Lam <$> (nt "lam" *> expr) <*> expr

pi' = Binder Pi <$> (nt "pi" *> expr) <*> expr

apply = Apply <$> expr <*> expr

nat = (Base (ANat Z) <$ nt "Z")
  <|> try (inc <$> (nt "(" *> nt "S" *> nat <* nt ")"))
  where inc (Base (ANat k)) = Base (ANat (S k))

form = lambda
  <|> pi'
  <|> apply
  <|> nat

expr = try nat <|> (nt "(" *> form <* nt ")") <|> base