module Main where

import Data.Monoid
import Test.Framework

import qualified Test.Parser as Parser
import qualified Test.Eval   as Eval

main :: IO ()
main = defaultMainWithOpts tests mempty

tests = concat
    [ Parser.tests
    , Eval.tests
    ]
