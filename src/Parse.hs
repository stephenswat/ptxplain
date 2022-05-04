module Parse (parsePtx) where

import Text.Megaparsec (Parsec, runParser, eof)
import Text.Megaparsec.Char (string)
import Control.Applicative (many)
import Data.Either (either)
import Data.Void (Void)

import Ptx (PtxProgram (..), PtxInstruction (..))

type Parser = Parsec Void String

pInstrLd :: Parser PtxInstruction
pInstrLd = string "ld" >> return LD

pInstr :: Parser PtxInstruction
pInstr = pInstrLd

pPtx :: Parser PtxProgram
pPtx = do
    instr <- many pInstr
    eof
    return PtxProgram
        { ptxMajorVersion = 8
        , ptxMinorVersion = 3
        , instructions = instr
        }

parsePtx :: String -> String -> PtxProgram
parsePtx fn fc = either (error . errorBundlePretty) id result
    where
        result = runParser pPtx fn fc
