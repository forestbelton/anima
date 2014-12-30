module Main where

import Anima.Types
import Anima.Eval
import Anima.Parser
import Anima.PPrint

import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO
import System.Console.ANSI
import System.Console.Haskeline

banner :: String
banner =  "   __    _  _  ____  __  __    __     \n"
       ++ "  /__\\  ( \\( )(_  _)(  \\/  )  /__\\    \n"
       ++ " /(__)\\  )  (  _)(_  )    (  /(__)\\   \n"
       ++ "(__)(__)(_)\\_)(____)(_/\\/\\_)(__)(__)  Version 0.1\n"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    putStrLn banner
    runInputT defaultSettings repl

green :: String -> String
green s = setSGRCode [SetColor Foreground Vivid Green] ++ s ++ setSGRCode []

topLevel :: String -> InputT IO ()
topLevel s = do
    if head s == '#'
        then return ()
        else do
            outputStrLn $ case parse expr "" s of
                Right e -> do
                    let reduced = beta [] e
                        ty      = typeOf [] reduced
                    (pprint reduced) ++ " : " ++ green (pprint ty)
                Left err   -> show err

repl :: InputT IO ()
repl = forever $ do
    input <- getInputLine "=> "
    case input of
        Nothing ->
            return ()
        Just line ->
            topLevel line