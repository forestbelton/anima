module Anima.PPrint (pprint) where

import Anima.Types

pprint :: Term -> String
pprint (Base Unit)      = "Unit"
pprint (Base TUnit)     = "TUnit"
pprint (Base Type)      = "Type"
pprint (Var x)          = show x
pprint (Binder Lam t m) = "λ:" ++ pprint t ++ " → (" ++ pprint m ++ ")"
pprint (Binder Pi t m)  = "∏:" ++ pprint t ++ " → (" ++ pprint m ++ ")"
pprint (Apply a b)      = "(" ++ pprint a ++ " " ++ pprint b ++ ")"