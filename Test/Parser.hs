module Test.Parser (tests) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Anima.Types
import Anima.Parser

import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Error (ParseError, errorMessages)

tests =
    [ testCase "can parse unit" parseUnit
    , testCase "can parse unit type" parseUnitTy
    , testCase "can parse type type" parseTyTy
    , testCase "can parse function () -> ()" parseIdFunc
    ]

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b

parser_test :: Term -> String -> Assertion
parser_test t s = case (Right t ~=? parse expr "" s) of
    TestCase as -> as

parseUnit   = parser_test (Base Unit) "Unit"
parseUnitTy = parser_test (Base TUnit) "TUnit"
parseTyTy   = parser_test (Base Type) "Type"
parseIdFunc = parser_test (Binder Lam (Base TUnit) (Base Unit)) "(lam TUnit Unit)"