module Main where

import Options.Applicative (execParser)

import CLI (Configuration (..), optParser)
import Parse (parsePtx)

main :: IO ()
main = do
    conf <- execParser optParser
    file <- readFile (fileName conf)
    let program = parsePtx (fileName conf) file
    putStrLn . show $ program
