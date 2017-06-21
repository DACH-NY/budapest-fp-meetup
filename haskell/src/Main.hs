module Main where

import System.IO
import System.Console.Readline
import Control.Monad
import Parser
import Eval
           
readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
   maybeLine <- readline "CEK>>> "
   case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do addHistory line
                    putStrLn . show  . run $ line
                    readEvalPrintLoop

    where run input = (tryParseExpr input) >>= evaluate


main :: IO ()
main = do
  _ <- putStrLn "Welcome to cek-scheme"
  readEvalPrintLoop

