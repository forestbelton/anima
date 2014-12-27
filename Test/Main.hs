module Main where

import Anima.Types
import Anima.Eval
import Anima.Parser

import Data.Maybe (catMaybes)
import Text.Printf (printf)
import System.Exit (exitSuccess, exitFailure)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

-- helper constructors
unit :: Term
unit = Base Unit

tunit :: Term
tunit = Base TUnit

eID :: Term -> Term
eID ty = Binder Lam ty (Var 0)

-- test harness
instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

check :: (Eq a, Show a) => String -> a -> a -> Maybe String
check err a b = if a == b
    then Nothing
    else Just (printf "'%s':\n\tgot      %s\n\texpected %s\n" err (show b) (show a))

-- evaluator tests
unitIdentity = check "identity for units" exp actual
    where exp    = eID tunit
          actual = Binder Lam tunit (Var 0)

unitTC = check "unit typechecks" exp actual
    where exp    = tunit
          actual = typeOf [] unit

appIdentityTC = check "application of identity typechecks" exp actual
    where exp    = tunit
          actual = typeOf [] (Apply (eID tunit) unit)

idThroughIdTC = check "id . id : unit -> unit" exp actual
    where exp    = Binder Pi tunit tunit
          actual = typeOf [] (Apply (eID (Binder Pi tunit tunit)) (eID tunit))

-- parser tests
parseUnit = check "can parse unit" exp actual
    where exp    = Right unit
          actual = parse expr "" "E"

parseUnitTy = check "can parse unit type" exp actual
    where exp    = Right tunit
          actual = parse expr "" "TUnit"

parseTyTy = check "can parse type type" exp actual
    where exp    = Right (Base Type)
          actual = parse expr "" "Type"

parseIdFunc = check "can parse function () -> ()" exp actual
    where exp    = Right (Binder Lam tunit unit)
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
