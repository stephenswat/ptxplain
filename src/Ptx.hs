module Ptx where

data ValueType
    = Unsigned64
    | Float32
    | Float64
    | Binary32
    | Binary64
    | Predicate
    deriving (Eq, Ord, Show)

data StateSpace
    = Param
    deriving (Eq, Ord, Show)

data PtxVariableRef = PtxVariableRef
    { variableRefName :: String
    }
    deriving (Eq, Ord, Show)

data PtxInstruction
    = LD (Maybe StateSpace) ValueType PtxVariableRef String
    | ST
    deriving (Eq, Ord, Show)

data PtxVariable = PtxVariable
    { variableType :: ValueType
    , variableName :: String
    , variableCount :: Integer
    }
    deriving (Show)

data PtxParameter = PtxParameter
    { parameterType :: ValueType
    , parameterName :: String
    }
    deriving (Show)

data PtxFunction = PtxFunction
    { name :: String
    , parameters :: [PtxParameter]
    , variables :: [PtxVariable]
    , instructions :: [PtxInstruction]
    }
    deriving (Show)

data PtxProgram = PtxProgram
    { ptxMajorVersion :: Integer
    , ptxMinorVersion :: Integer
    , target :: String
    , addressSize :: Integer
    , functions :: [PtxFunction]
    }
    deriving (Show)
