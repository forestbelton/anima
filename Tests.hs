module Main where

import Anima.Types
import Anima.Eval
import Anima.Parser

import Data.Maybe (catMaybes)
import Text.Printf (printf)
import System.Exit (exitSuccess, exitFailure)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

check :: (Eq a, Show a) => String -> a -> a -> Maybe String
check err a b = if a == b
    then Nothing
    else Just (printf "'%s':\n\tgot      %s\n\texpected %s\n" err (show b) (show a))

-- evaluator tests
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

-- parser tests
parseUnit = check "can parse unit" exp actual
    where exp    = Right EUnit
          actual = parse expr "" "E"

parseUnitTy = check "can parse unit type" exp actual
    where exp    = Right TUnit
          actual = parse expr "" "TUnit"

parseTyTy = check "can parse type type" exp actual
    where exp    = Right TType
          actual = parse expr "" "Type"

parseIdFunc = check "can parse function () -> ()" exp actual
    where exp    = Right (EAbs TUnit EUnit)
          actual = parse expr "" "(lam TUnit E)"

checks = [
    unitIdentity,
    appIdentityTC,
    idThroughIdTC,

    parseUnitTy,
    parseTyTy,
    parseIdFunc
  ]

main = do
    let failures = catMaybes checks
    mapM_ putStrLn failures
    if length failures == 0
        then exitSuccess
        else exitFailure
