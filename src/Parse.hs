{-# LANGUAGE FlexibleInstances #-}

module Parse (parsePtx) where

import Text.Megaparsec (Parsec, VisualStream(..), runParser, eof, errorBundlePretty, token, empty, choice, takeWhileP, try, sepBy, sepEndBy)
import Text.Megaparsec.Char (string, newline, space1, alphaNumChar, digitChar, char, letterChar, punctuationChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, some, (<|>))
import Data.Either (either)
import Data.Void (Void)

import Ptx (PtxProgram (..), PtxFunction (..), PtxInstruction (..), PtxParameter (..), PtxVariable (..), ValueType (..), StateSpace (..), PtxVariableRef (..))

type Parser = Parsec Void String

lSpace :: Parser ()
lSpace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lLexeme = L.lexeme lSpace
lInteger = lLexeme L.decimal

pComment :: Parser ()
pComment = do
    string "//"
    str <- takeWhileP (Just "non-newline character") (/= '\n')
    newline
    lSpace

pDirectiveVersion :: Parser (Integer, Integer)
pDirectiveVersion = do
    string ".version"
    lSpace
    major <- some digitChar
    char '.'
    minor <- some digitChar
    lSpace
    return (read major, read minor)

pDirectiveTarget :: Parser String
pDirectiveTarget = do
    string ".target"
    lSpace
    str <- some (alphaNumChar <|> char '_')
    lSpace
    return str

pDirectiveAddressSize :: Parser Integer
pDirectiveAddressSize = do
    string ".address_size"
    lSpace
    size <- some digitChar
    lSpace
    return (read size)

pIdentifier :: Parser String
pIdentifier = pCaseLetter <|> pCaseSymbol
    where
        pFollowsym :: Parser Char
        pFollowsym = alphaNumChar <|> char '_' <|> char '$'
        pCaseLetter = do
            head <- letterChar
            tail <- many pFollowsym
            return (head:tail)
        pCaseSymbol = do
            head <- (char '_' <|> char '$' <|> char '%')
            tail <- some pFollowsym
            return (head:tail)

pValueType :: Parser ValueType
pValueType = choice
    [ string "u64" >> return Unsigned64
    , string "f32" >> return Float32
    , string "f64" >> return Float64
    , string "b32" >> return Binary32
    , string "b64" >> return Binary64
    , string "pred" >> return Predicate
    ]

pStateSpace :: Parser StateSpace
pStateSpace = choice
    [ string "param" >> return Param
    ]

pStateSpaceSep :: Parser (Maybe StateSpace)
pStateSpaceSep = (char '.' >> Just <$> pStateSpace) <|> (return Nothing)

pParameter :: Parser PtxParameter
pParameter = do
    string ".param"
    lSpace
    string "."
    parameterType' <- pValueType
    lSpace
    parameterName' <- pIdentifier
    lSpace
    return PtxParameter
        { parameterType = parameterType'
        , parameterName = parameterName'
        }

pVariable :: Parser PtxVariable
pVariable = do
    string ".reg"
    lSpace
    char '.'
    variableType' <- pValueType
    lSpace
    ('%':variableName') <- pIdentifier
    char '<'
    variableCount' <- lInteger
    char '>'
    lSpace
    return PtxVariable
        { variableType = variableType'
        , variableName = variableName'
        , variableCount = variableCount'
        }

pVariableRef :: Parser PtxVariableRef
pVariableRef = do
    ('%':variableRefName') <- pIdentifier
    return PtxVariableRef
        { variableRefName = variableRefName'
        }

pInstructionLd :: Parser PtxInstruction
pInstructionLd = do
    string "ld"
    statespace' <- pStateSpaceSep
    char '.'
    instructionLdType' <- pValueType
    lSpace
    dstRef' <- pVariableRef
    lSpace
    char ','
    lSpace
    char '['
    lSpace
    srcRef' <- pIdentifier
    lSpace
    char ']'
    return (LD statespace' instructionLdType' dstRef' srcRef')

pInstruction :: Parser PtxInstruction
pInstruction = choice
    [ pInstructionLd
    ]

pFunction :: Parser PtxFunction
pFunction = do
    string ".visible"
    lSpace
    string ".entry"
    lSpace
    name' <- pIdentifier
    lSpace
    char '('
    lSpace
    parameters' <- sepBy pParameter (lSpace >> char ',' >> lSpace)
    lSpace
    char ')'
    lSpace
    char '{'
    lSpace
    variables' <- sepEndBy pVariable (lSpace >> char ';' >> lSpace)
    lSpace
    instructions' <- sepEndBy pInstruction (lSpace >> char ';' >> lSpace)
    char ';'
    lSpace
    return PtxFunction
        { name = name'
        , parameters = parameters'
        , variables = variables'
        , instructions = instructions'
        }

pPtx :: Parser PtxProgram
pPtx = do
    lSpace
    (ptxMajorVersion', ptxMinorVersion') <- pDirectiveVersion
    target' <- pDirectiveTarget
    addressSize' <- pDirectiveAddressSize
    functions' <- many pFunction
    eof
    return PtxProgram
        { ptxMajorVersion = ptxMajorVersion'
        , ptxMinorVersion = ptxMinorVersion'
        , target = target'
        , addressSize = addressSize'
        , functions = functions'
        }

parsePtx :: String -> String -> PtxProgram
parsePtx fn fc = either (error . errorBundlePretty) id parserResult
    where
        parserResult = runParser pPtx fn fc
