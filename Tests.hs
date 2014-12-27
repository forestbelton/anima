module Main where

import Anima.Types
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

unitTC = check "unit typechecks" exp actual
    where exp    = Just TUnit
          actual = typeOf [] EUnit

appIdentityTC = check "application of identity typechecks" exp actual
    where exp    = Just TUnit
          actual = typeOf [] (EApp (eID TUnit) EUnit)

idThroughIdTC = check "id . id : unit -> unit" exp actual
    where exp    = Just (EPi TUnit TUnit)
          actual = typeOf [] (EApp (eID (EPi TUnit TUnit)) (eID TUnit))

checks = [
    unitIdentity,
    appIdentityTC,
    idThroughIdTC
  ]

main = do
    let failures = catMaybes checks
    mapM_ putStrLn failures
    if length failures == 0
        then exitSuccess
        else exitFailure
