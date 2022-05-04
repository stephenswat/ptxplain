module CLI (Configuration (..), optParser) where

import Options.Applicative (
        Parser, InfoMod, ParserInfo, long, metavar, help, strOption, info, helper,
        header, progDesc, fullDesc
    )
import Control.Applicative ((<**>))

data Configuration = Configuration
  { fileName :: String
  }

optParserParser :: Parser Configuration
optParserParser = Configuration <$>
    strOption (
        long "input" <>
        metavar "INPUT.ptx" <>
        help "PTX file to load for analysis"
    )

optParserInfo :: InfoMod Configuration
optParserInfo = fullDesc <>
    header "ptxplain - a tool for understanding NVIDIA PTX" <>
    progDesc "Read in a PTX file and enter interactive analysis"

optParser :: ParserInfo Configuration
optParser = info (optParserParser <**> helper) optParserInfo
