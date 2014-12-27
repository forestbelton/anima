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
TE            -- The type of the unit type
Type          -- The type of types

Special forms:

(Lam ty expr) -- s     -> t
(Pi ty expr)  -- (x:t) -> M x

-}

ws = many (oneOf " \r\t\n")
nt p = string p <* ws

base = (EUnit <$ nt "E")
  <|> try (TUnit <$ nt "TE")
  <|> (TType <$ nt "Type")

lambda = EAbs <$> (nt "lam" *> expr) <*> expr

form = lambda

expr = (nt "(" *> form <* nt ")") <|> base