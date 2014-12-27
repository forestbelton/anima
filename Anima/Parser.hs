{-# LANGUAGE NoMonomorphismRestriction #-}

module Anima.Parser where

import Anima.Types
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec

{-

General syntax, a la LISP: (fn arg1)

0, 1, 2, ... represent De Bruijn indices

Base values:
E             -- The value of the unit type
TUnit         -- The type of the unit type
Type          -- The type of types

Special forms:

(Lam ty expr) -- s     -> t
(Pi ty expr)  -- (x:t) -> M x
(N expr)      -- (N expr) where N = 0, 1, ...

-}


ws = many (oneOf " \r\t\n")
nt p = string p <* ws

base = (EUnit <$ nt "E")
  <|> try (TUnit <$ nt "TUnit")
  <|> (TType <$ nt "Type")
  <|> (EVar . read <$> (many1 (oneOf "0123456789") <* ws))

lambda = EAbs <$> (nt "lam" *> expr) <*> expr

pi' = EPi <$> (nt "pi" *> expr) <*> expr

apply = EApp <$> expr <*> expr

form = lambda
  <|> pi'
  <|> apply

expr = (nt "(" *> form <* nt ")") <|> base