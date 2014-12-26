module Tests where

import Eval

import Data.Maybe
import Text.Printf

check :: (Eq a, Show a) => String -> a -> a -> Maybe String
check err a b = if a == b
    then Nothing
    else Just (printf "'%s':\n\tgot      %s\n\texpected %s\n" err (show b) (show a))

unitIdentity = check "identity for units" exp actual
    where exp    = eID TUnit
          actual = EAbs TUnit (EVar 0)

appIdentityTC = check "application of identity typechecks" exp actual
    where exp    = Just (TFun TUnit TUnit)
          actual = typeOf [] (EApp (eID TUnit) EUnit)

checks = [
    unitIdentity,
    appIdentityTC
  ]

failures = mapM_ putStr (catMaybes checks)