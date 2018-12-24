module Parser
    ( parseDataInfo
    , parseDataType
    , parseMemberName
    , parseDataTypeRef
    ) where

import Control.Applicative
import Text.Trifecta

data Test =  String | Unit

data MemberName = MemberName String deriving (Eq, Ord, Show)
data MemberType = MemberType String deriving (Eq, Ord, Show)
data DataTypeName = DataTypeName String deriving (Eq, Ord, Show)

data DataTypeRef = DataTypeRef {
      memberName :: MemberName
    , memberType :: MemberType
    } deriving (Eq, Ord, Show)

data DataTypeExpr 
    = SumExpr [MemberType]             -- data MySumType { A | B | C | ...}
    | ProductExpr [DataTypeRef]        -- data MyProductType { a :: A, b :: B, ...}
    deriving (Eq, Ord, Show)

data DataTypeInfoExpr = DataTypeInfoExpr DataTypeName DataTypeExpr deriving (Eq, Ord, Show)

parseMemberIdentifier :: Parser [Char]
parseMemberIdentifier = do 
    l <- lower 
    s <- many alphaNum
    _ <- spaces
    pure (l : s)

parseTypeNameIdentifier :: Parser [Char]
parseTypeNameIdentifier = do 
    l <- upper 
    s <- many alphaNum
    _ <- spaces
    pure (l : s)

parseMemberName = MemberName <$> parseMemberIdentifier
parseMemberType = MemberType <$> parseTypeNameIdentifier
parseDataTypeName = DataTypeName <$> parseTypeNameIdentifier

parseDataTypeRef :: Parser DataTypeRef
parseDataTypeRef = DataTypeRef <$> parseMemberName <* (symbol "::") <*> parseMemberType

parseProduct :: Parser DataTypeExpr
parseProduct = ProductExpr <$> parseDataTypeRef `sepBy1` comma

parseSum :: Parser DataTypeExpr
parseSum = SumExpr <$> parseMemberType `sepBy1` (symbol "|")

parseDataType :: Parser DataTypeExpr
parseDataType = parseProduct <|> parseSum

parseDataInfo :: Parser DataTypeInfoExpr
parseDataInfo = DataTypeInfoExpr <$> ((symbol "data") *> parseDataTypeName) <*> braces parseDataType
    
    