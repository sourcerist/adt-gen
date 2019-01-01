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
    indent ++ "public class " ++ dataTypeName dName ++ "\n" ++
    indent ++ "{\n" ++ contents exprType ++ indent ++ "}\n" 
    where 
        nextIndent = (head indent) : indent
        nextIndent2 = (head nextIndent) : nextIndent
        contents (ProductExpr dataRefs) =
            intercalate ("\n" ++ nextIndent) (map declLine dataRefs) ++ 
            nextIndent ++ "public " ++ dataTypeName dName ++ "(" ++ dParams ++ ") => (" ++ declNamesCommaDelim ++ ") = (" ++ dParams ++ ");" ++ 
            indent ++ "}" 
            where 
                declLine :: DataTypeRef -> String
                declLine d = "public " ++ dataTypeName (memberType d) ++ " " ++ (upperCamelCase . memberNameString . memberName) d ++ "{ get; }\n"
                dParams = intercalate ", " $ map (lowerCamelCase . memberNameString . memberName) dataRefs
                declNamesCommaDelim = intercalate ", " $ map (upperCamelCase . memberNameString . memberName) dataRefs
        contents (SumExpr dataTypeNames) =
            intercalate ("\n" ++ nextIndent) (map derivedClass dataTypeNames)
            where 
                derivedClass :: DataTypeName -> String
                derivedClass d = 
                    nextIndent ++ "public virtual bool Is" ++ dataTypeName d ++ " { get; } = false;\n" ++
                    nextIndent ++ "public class _" ++ dataTypeName d ++ " : " ++ dataTypeName dName ++ "\n" ++
                    nextIndent ++ "{\n" ++
                    nextIndent2 ++ "public " ++ dataTypeName d ++ " Value { get; }\n" ++
                    nextIndent2 ++ "public _" ++ dataTypeName d ++ "(" ++ dataTypeName d ++ " value) => Value = value;" ++
                    nextIndent2 ++ "public override bool Is" ++ dataTypeName d ++ " { get; } = true;\n" ++
                    nextIndent ++ "}\n" 
        
evalCodeGenTree :: CodeGenTree -> String
evalCodeGenTree tree =
    let classes = map (\x -> codeGenDataTypeRef x "\t") (dataTypes tree) in
    intercalate "\n\n" classes