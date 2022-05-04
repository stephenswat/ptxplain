module Main where

import Options.Applicative (execParser)

import CLI (Configuration (..), optParser)

main :: IO ()
main = (execParser optParser) >>= (putStrLn . show . fileName)
