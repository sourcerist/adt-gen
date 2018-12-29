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

parseNamespace :: Parser Namespace
parseNamespace = Namespace <$> many (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "._")

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
parseDataInfo = DataTypeInfoExpr <$> ((symbol "data") *> parseDataTypeName) <*> braces parseDataType <* whiteSpace

parseImports :: Parser [Namespace]
parseImports = many (symbol "include" *> whiteSpace *> parseNamespace <* char ';' <* whiteSpace)

parseDataFile :: Parser CodeGenTree
parseDataFile = CodeGenTree <$> parseImports <* (symbol "namespace") <* whiteSpace <*> parseNamespace <* whiteSpace <*> (braces (many parseDataInfo)) <* eof
    