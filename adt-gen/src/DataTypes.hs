module DataTypes where
    

data MemberName = MemberName String deriving (Eq, Ord, Show)
data MemberType = MemberType String deriving (Eq, Ord, Show)
data DataTypeName = DataTypeName String deriving (Eq, Ord, Show)
data Namespace = Namespace String deriving (Eq, Ord, Show)

data CodeGenTree = CodeGenTree {
      imports   :: [Namespace]
    , namespace :: Namespace
    , dataTypes :: [DataTypeInfoExpr]
    } deriving (Eq, Ord, Show)

data DataTypeRef = DataTypeRef {
      memberName :: MemberName
    , memberType :: MemberType
    } deriving (Eq, Ord, Show)

data DataTypeExpr 
    = SumExpr [MemberType]             -- data MySumType { A | B | C | ...}
    | ProductExpr [DataTypeRef]        -- data MyProductType { a :: A, b :: B, ...}
    deriving (Eq, Ord, Show)

data DataTypeInfoExpr = DataTypeInfoExpr DataTypeName DataTypeExpr deriving (Eq, Ord, Show)