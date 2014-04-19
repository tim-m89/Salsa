module Main where

import System.Environment (getArgs, getProgName)
import Parser

outputUsage :: IO ()
outputUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [input-files]"



main :: IO ()
main = do
    args <- getArgs
    if null args then
        outputUsage
    else outputUsage

