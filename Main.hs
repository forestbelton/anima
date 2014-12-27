module Main where

import Anima.Types
import Anima.Eval
import Anima.Parser

import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO

banner :: String
banner =  "   __    _  _  ____  __  __    __     \n"
       ++ "  /__\\  ( \\( )(_  _)(  \\/  )  /__\\    \n"
       ++ " /(__)\\  )  (  _)(_  )    (  /(__)\\   \n"
       ++ "(__)(__)(_)\\_)(____)(_/\\/\\_)(__)(__)  Version 0.1\n"

main :: IO ()
main = do
    putStrLn banner
    repl

repl :: IO ()
repl = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "> "
    line <- getLine
    case parse expr "" line of
        Right e -> do
            case typeOf [] e of
                Just ty ->
                    putStrLn $ (show (beta e)) ++ " : " ++ (show ty)
                Nothing ->
                    putStrLn "Failed to typecheck"
        Left err   -> print err
