module Main where

import Anima.Eval

import Data.Maybe (catMaybes)
import Text.Printf (printf)
import System.Exit (exitSuccess, exitFailure)

check :: (Eq a, Show a) => String -> a -> a -> Maybe String
check err a b = if a == b
    then Nothing
    else Just (printf "'%s':\n\tgot      %s\n\texpected %s\n" err (show b) (show a))

unitIdentity = check "identity for units" exp actual
    where exp    = eID TUnit
          actual = EAbs TUnit (EVar 0)

appIdentityTC = check "application of identity typechecks" exp actual
    where exp    = Just TUnit
          actual = typeOf [] (EApp (eID TUnit) EUnit)

checks = [
    unitIdentity,
    appIdentityTC
  ]

main = do
    let failures = catMaybes checks
    mapM_ putStrLn failures
    if length failures == 0
        then exitSuccess
        else exitFailure
