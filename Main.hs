module Main where

import Anima.Types
import Anima.Eval
import Anima.Parser
import Anima.PPrint

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

topLevel :: String -> IO ()
topLevel s = do
    if head s == '#'
        then return ()
        else do
            case parse expr "" s of
                Right e -> do
                    let ty = typeOf [] e
                    putStrLn $ (pprint $ beta [] e) ++ " : " ++ (pprint ty)
                Left err   -> print err

repl :: IO ()
repl = forever $ do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    putStr "=> "
    line <- getLine
    topLevel line