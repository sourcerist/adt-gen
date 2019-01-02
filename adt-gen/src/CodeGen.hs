module CodeGen (test, dataTypeName, codeGenDataTypeRef, evalCodeGenTree) where

import DataTypes
import Data.List
import Data.Char

test = "asdf"

dataTypeName :: DataTypeName -> String
dataTypeName d = (name d) ++ showGenerics (genericParams d) where 
    showGenerics :: Maybe [DataTypeName] -> String
    showGenerics (Just x) = '<' : (intercalate ", " (map dataTypeName x)) ++ ">"
    showGenerics _ = ""

dataTypeNameFlattenGenerics :: DataTypeName -> String
dataTypeNameFlattenGenerics d = filter noAngles (dataTypeName d)
    where noAngles c = (c /= '<') && (c /= '>')

memberNameString :: MemberName -> String
memberNameString (MemberName ms) = ms

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f s = f (head s) : tail s

lowerCamelCase :: String -> String
lowerCamelCase = mapFirst toLower

upperCamelCase :: String -> String
upperCamelCase = mapFirst toUpper

codeGenDataTypeRef :: DataTypeInfoExpr -> String -> String
codeGenDataTypeRef (DataTypeInfoExpr dName exprType) indent = 
    indent ++ "public partial class " ++ dataTypeName dName ++ "\n" ++
    indent ++ "{\n" ++ 
    contents exprType ++ "\n" ++ idLens ++ 
    indent ++ "}\n\n" ++ 
    indent ++ "public static partial class " ++ dataTypeName dName ++ "Optics\n" ++
    indent ++ "{\n" ++ 
    opticsExtensions exprType ++
    indent ++ "}\n"
    where 
        idLensParams = intercalate ", " $ (take 4 . repeat . dataTypeName) dName
        idLens = nextIndent ++ "public static AlgebraicDataTypes.Optics.Lens<" ++ idLensParams ++ "> Id { get; } = AlgebraicDataTypes.Optics.Lens.Create<" ++ idLensParams ++ ">(s => s, (a2b,s) => a2b(s));\n"
        nextIndent = (head indent) : indent
        nextIndent2 = (head nextIndent) : nextIndent
        contents (ProductExpr dataRefs) =
            intercalate "\n" (map declLine dataRefs) ++ "\n\n" ++
            nextIndent ++ "public " ++ dataTypeName dName ++ "(" ++ dParams1 ++ ") => (" ++ declNamesCommaDelim ++ ") = (" ++ dParams2 ++ ");\n"
            where 
                declLine :: DataTypeRef -> String
                declLine d = nextIndent ++ "public " ++ dataTypeName (memberType d) ++ " " ++ (upperCamelCase . memberNameString . memberName) d ++ "{ get; }"
                dParams1 = intercalate ", " $ map (\dr -> (dataTypeName . memberType) dr ++ " " ++ (lowerCamelCase . memberNameString . memberName) dr) dataRefs
                dParams2 = intercalate ", " $ map (lowerCamelCase . memberNameString . memberName) dataRefs
                declNamesCommaDelim = intercalate ", " $ map (upperCamelCase . memberNameString . memberName) dataRefs
        contents (SumExpr dataTypeNames) =
            intercalate "\n" (map derivedClass dataTypeNames)
            where 
                derivedClass :: DataTypeName -> String
                derivedClass d = 
                    nextIndent ++ "public virtual bool Is" ++ dataTypeNameFlattenGenerics d ++ " { get => false; }\n" ++
                    nextIndent ++ "public static _" ++ dataTypeNameFlattenGenerics d ++ " Create(" ++ dataTypeName d ++ " val) => new _" ++ dataTypeNameFlattenGenerics d ++ "(val);\n" ++ 
                    nextIndent ++ "public partial class _" ++ dataTypeNameFlattenGenerics d ++ " : " ++ dataTypeName dName ++ "\n" ++
                    nextIndent ++ "{\n" ++
                    nextIndent2 ++ "public " ++ dataTypeName d ++ " Value { get; }\n" ++
                    nextIndent2 ++ "public _" ++ dataTypeNameFlattenGenerics d ++ "(" ++ dataTypeName d ++ " value) => Value = value;\n" ++
                    nextIndent2 ++ "public override bool Is" ++ dataTypeNameFlattenGenerics d ++ " { get => true; }\n" ++
                    nextIndent ++ "}\n" 
        opticsExtensions (ProductExpr dataRefs) = 
            intercalate "\n" (map memberOptics dataRefs)
            where 
                mName = (upperCamelCase . memberNameString . memberName)
                lensName x = mName x ++ "Lens"
                s = dataTypeName dName
                typeFor = (dataTypeName . memberType)
                altConstructor d = "new " ++ s ++ "(" ++ intercalate ", " (map (\x -> if x == d then "s2a(s." ++ mName x ++ ")" else "s." ++ mName x) dataRefs) ++ ")"
                memberOptics :: DataTypeRef -> String
                memberOptics d =
                    nextIndent ++ "public static Lens<" ++ s ++ ", " ++ s ++ ", " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ lensName d ++ " = Lens.Create<" ++ s ++ ", " ++ s ++ ", " ++ typeFor d ++ ", " ++ typeFor d ++ ">(s => s." ++ mName d ++ ", (s2a, s) => " ++ altConstructor d ++ ");\n" ++
                    nextIndent ++ "public static ILens<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ILens<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    nextIndent ++ "public static ITraversal<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ITraversal<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    nextIndent ++ "public static ISetter<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ISetter<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    nextIndent ++ "public static IGetter<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IGetter<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    nextIndent ++ "public static IFold<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IFold<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n"
        opticsExtensions (SumExpr dataTypeNames) = 
            intercalate "\n" (map memberPrism dataTypeNames)
            where 
                prismName x = dataTypeNameFlattenGenerics x ++ "Prism"
                s = dataTypeName dName
                eitherName d = "Either<" ++ dataTypeName dName ++ ", " ++ dataTypeName d ++ ">"
                memberPrism :: DataTypeName -> String
                memberPrism d = 
                    nextIndent ++ "public static Prism<" ++ s ++ ", " ++ s ++ ", " ++ dataTypeName d ++ ", " ++ dataTypeName d ++ "> " ++ prismName d ++ " = Prism.Create<" ++ s ++ ", " ++ s ++ ", " ++ dataTypeName d ++ ", " ++ dataTypeName d ++ ">(\n" ++
                    nextIndent2 ++ "b => " ++ s ++ ".Create(b),\n" ++
                    nextIndent2 ++ "s => { if (s is " ++ s ++ "._" ++ dataTypeNameFlattenGenerics d ++ " a) return new " ++ eitherName d ++ ".Right(a.Value); else return new " ++ eitherName d ++ ".Left(s); });" ++
                    nextIndent ++ "public static IPrism<S,T, " ++ dataTypeName d ++ ", " ++ dataTypeName d ++ "> " ++ dataTypeNameFlattenGenerics d ++ "<S,T>(this IPrism<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ prismName d ++ ");\n" ++
                    nextIndent ++ "public static ITraversal<S,T, " ++ dataTypeName d ++ ", " ++ dataTypeName d ++ "> " ++ dataTypeNameFlattenGenerics d ++ "<S,T>(this ITraversal<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ prismName d ++ ");\n" ++
                    ""
                    --nextIndent ++ "public static ISetter<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ISetter<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    --nextIndent ++ "public static IGetter<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IGetter<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n" ++
                    --nextIndent ++ "public static IFold<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IFold<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");\n"

namespaceString :: Namespace -> String
namespaceString (Namespace ns) = ns

importNamespaces :: [Namespace] -> String
importNamespaces n = intercalate "\n" $ map (\ns -> "using " ++ namespaceString ns ++ ";") n
        
evalCodeGenTree :: CodeGenTree -> String
evalCodeGenTree tree =
    importNamespaces (imports tree) ++ "\n\n" ++
    "namespace " ++ namespaceString (namespace tree) ++ "\n" ++
    "{\n" ++
    intercalate "\n\n" classes ++
    "}\n" where
        classes = map (\x -> codeGenDataTypeRef x "\t") (dataTypes tree) 