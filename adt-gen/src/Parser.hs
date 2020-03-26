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
    u <- upper 
    s <- many alphaNum
    _ <- spaces
    pure (u : s)

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

parseSumMember :: Parser DataTypeRef
parseSumMember = do
    name <- parseTypeNameIdentifier 
    (symbol "::") 
    dtName <- parseDataTypeName
    pure (DataTypeRef (MemberName name) dtName)

parseSimpleMember :: Parser DataTypeRef
parseSimpleMember = do
    name <- parseTypeNameIdentifier
    pure (DataTypeRef (MemberName name) (DataTypeName "Unit" Nothing))

parseSum :: Parser DataTypeExpr
parseSum = SumExpr <$> p `sepBy1` (symbol "|") where p = choice [try parseSumMember, parseSimpleMember]

parseDataType :: Parser DataTypeExpr
parseDataType = (try parseProduct) <|> parseSum

parseDataInfo :: Parser DataTypeInfoExpr
parseDataInfo = DataTypeInfoExpr <$> ((symbol "data") *> parseDataTypeName) <*> braces parseDataType <* whiteSpace

parseImports :: Parser [Namespace]
parseImports = many (symbol "using" *> whiteSpace *> parseNamespace <* char ';' <* whiteSpace)

parseDataFile :: Parser CodeGenTree
parseDataFile = CodeGenTree <$> parseImports <* (symbol "namespace") <* whiteSpace <*> parseNamespace <* whiteSpace <*> (braces (many parseDataInfo)) <* eof
    