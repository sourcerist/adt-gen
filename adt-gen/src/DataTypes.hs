module DataTypes where
    

data MemberName = MemberName String deriving (Eq, Ord, Show)
data DataTypeName = DataTypeName {
    name          :: String
  , genericParams :: Maybe [DataTypeName]
  } deriving (Eq, Ord, Show)
data Namespace = Namespace String deriving (Eq, Ord, Show)

data CodeGenTree = CodeGenTree {
      imports   :: [Namespace]
    , namespace :: Namespace
    , dataTypes :: [DataTypeInfoExpr]
    } deriving (Eq, Ord, Show)

data DataTypeRef = DataTypeRef {
      memberName :: MemberName
    , memberType :: DataTypeName
    } deriving (Eq, Ord, Show)

data DataTypeExpr 
    = SumExpr [DataTypeName]           -- data MySumType { A | B | C | ...}
    | ProductExpr [DataTypeRef]        -- data MyProductType { a :: A, b :: B, ...}
    deriving (Eq, Ord, Show)

data DataTypeInfoExpr = DataTypeInfoExpr DataTypeName DataTypeExpr deriving (Eq, Ord, Show)