module Parser
    ( parseNameIdentifier
    , parseMemberNameIdentifier
    ) where

import Control.Applicative
import Text.Trifecta

data Test =  String | Unit

data MemberName = MemberName String deriving (Eq, Ord, Show)
data MemberType = MemberType String deriving (Eq, Ord, Show)

data DataTypeRef = DataTypeRef {
      memberName :: MemberName
    , memberType :: MemberType
    } deriving (Eq, Ord, Show)

data DataTypeExpr
    = SumExpr [DataTypeRef]        -- data MySumType { a :: A | B | C }
    | ProductExpr [DataTypeRef]    -- data MyProductType { a :: A, b :: B, ...}
    deriving (Eq, Ord, Show)

parseNameIdentifier :: Parser [Char]
parseNameIdentifier = do 
    l <- lower 
    s <- many alphaNum
    _ <- spaces
    pure (l : s)

parseMemberNameIdentifier = MemberName <$> parseNameIdentifier