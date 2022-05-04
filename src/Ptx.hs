module Ptx where

data PtxInstruction
    = LD
    | ST
    deriving (Eq, Ord, Show)

data PtxProgram = PtxProgram
    { ptxMajorVersion :: Integer
    , ptxMinorVersion :: Integer
    , instructions :: [PtxInstruction]
    }
    deriving (Show)
