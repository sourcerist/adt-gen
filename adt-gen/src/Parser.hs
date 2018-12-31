module Parser
    ( parseDataFile
    , parseImports
    , parseDataInfo
    , parseDataType
    , parseMemberName
    , parseDataTypeRef
    ) where

import Control.Applicative
import Text.Trifecta
import DataTypes

parseMemberIdentifier :: Parser String
parseMemberIdentifier = do 
    l <- lower 
    s <- many alphaNum
    _ <- spaces
    pure (l : s)

parseTypeNameIdentifier :: Parser String
parseTypeNameIdentifier = do 
    l <- upper 
    s <- many alphaNum
    _ <- spaces
    pure (l : s)

parseNamespace :: Parser Namespace
parseNamespace = Namespace <$> many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "._")

parseDataTypeName :: Parser DataTypeName
parseDataTypeName = DataTypeName <$> parseTypeNameIdentifier <*> optional parseGenericTypes

parseGenericTypes :: Parser [DataTypeName]
parseGenericTypes = angles (commaSep1 parseDataTypeName)

parseMemberName :: Parser MemberName
parseMemberName = MemberName <$> parseMemberIdentifier

parseDataTypeRef :: Parser DataTypeRef
parseDataTypeRef = DataTypeRef <$> parseMemberName <* (symbol "::") <*> parseDataTypeName

parseProduct :: Parser DataTypeExpr 
parseProduct = ProductExpr <$> parseDataTypeRef `sepBy1` comma

parseSum :: Parser DataTypeExpr
parseSum = SumExpr <$> parseDataTypeName `sepBy1` (symbol "|")

parseDataType :: Parser DataTypeExpr
parseDataType = parseProduct <|> parseSum

parseDataInfo :: Parser DataTypeInfoExpr
parseDataInfo = DataTypeInfoExpr <$> ((symbol "data") *> parseDataTypeName) <*> braces parseDataType <* whiteSpace

parseImports :: Parser [Namespace]
parseImports = many (symbol "include" *> whiteSpace *> parseNamespace <* char ';' <* whiteSpace)

parseDataFile :: Parser CodeGenTree
parseDataFile = CodeGenTree <$> parseImports <* (symbol "namespace") <* whiteSpace <*> parseNamespace <* whiteSpace <*> (braces (many parseDataInfo)) <* eof
    