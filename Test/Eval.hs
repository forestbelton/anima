module Test.Eval (tests) where

import Test.Framework.Providers.HUnit
import Test.HUnit

import Anima.Types
import Anima.Eval

tests =
    [ testCase "can typecheck unit" unitTC
    , testCase "can typecheck unit type" tunitTC
    , testCase "can typecheck type type" ttypeTC
    , testCase "can typecheck application of () to () -> ()" appIdToUnitTC
    , testCase "can typecheck identity applied to identity" idOfIdTC
    ]

tc_test :: Term -> Term -> Assertion
tc_test ty t = case ty ~=? typeOf [] t of
    TestCase as -> as

unit   = Base Unit
tunit  = Base TUnit
ttype  = Base Type
eID ty = Binder Lam ty (Var 0)

unitTC        = tc_test tunit unit
tunitTC       = tc_test ttype tunit
ttypeTC       = tc_test ttype ttype
appIdToUnitTC = tc_test unit (Apply (eID tunit) unit)
idOfIdTC      = tc_test (eID tunit) (Apply (eID (Binder Pi tunit tunit)) (eID tunit))
