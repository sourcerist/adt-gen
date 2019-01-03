{-# LANGUAGE TemplateHaskell #-}

module DataTypes where

import Control.Lens

newtype MemberName = MemberName String deriving (Eq, Ord, Show)

data DataTypeName = DataTypeName {
    _name          :: String
  , _genericParams :: Maybe [DataTypeName]
  } deriving (Eq, Ord, Show)
  
newtype Namespace = Namespace String deriving (Eq, Ord, Show)

data CodeGenTree = CodeGenTree {
      _imports   :: [Namespace]
    , _namespace :: Namespace
    , _dataTypes :: [DataTypeInfoExpr]
    } deriving (Eq, Ord, Show)

data DataTypeRef = DataTypeRef {
      _memberName :: MemberName
    , _memberType :: DataTypeName
    } deriving (Eq, Ord, Show)

data DataTypeExpr 
    = SumExpr [DataTypeName]           -- data MySumType { A | B | C | ...}
    | ProductExpr [DataTypeRef]        -- data MyProductType { a :: A, b :: B, ...}
    deriving (Eq, Ord, Show)

data DataTypeInfoExpr = DataTypeInfoExpr DataTypeName DataTypeExpr deriving (Eq, Ord, Show)

makeLenses ''DataTypeName
makeLenses ''CodeGenTree
makeLenses ''DataTypeRef
makeLenses ''DataTypeExpr
makeLenses ''DataTypeInfoExpr