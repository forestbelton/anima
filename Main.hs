module Main where

import Anima.Eval

eval :: Expr -> IO ()
eval e = case typeOf [] e of
    Nothing -> putStrLn "Failed to typecheck"
    _       -> print $ beta e

main :: IO ()
main = do
    eval (EApp (eID TUnit) unit)