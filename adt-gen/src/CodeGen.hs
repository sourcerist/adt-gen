{-# LANGUAGE TemplateHaskell #-}

module CodeGen where

import DataTypes
import Data.List
import Data.Char
import Control.Lens
import Text.Printf

data Line = Line { _indent :: String, _text :: [String] } deriving (Eq, Ord, Show)
makeLenses ''Line

dataTypeName :: DataTypeName -> String
dataTypeName d = d^.name ++ d ^.genericParams ^.to concat ^.to showGenerics where 
    showGenerics x@(h:t) ='<' : x ^.to (map dataTypeName) ^.to (intercalate ", ") ++ ">"
    showGenerics _ = ""

dataTypeNameFlattenGenerics :: DataTypeName -> String
dataTypeNameFlattenGenerics d = filter noAngles (dataTypeName d)
    where noAngles c = (c /= '<') && (c /= '>')

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f s = f (head s) : tail s

lowerCamelCase :: String -> String
lowerCamelCase = mapFirst toLower

upperCamelCase :: String -> String
upperCamelCase = mapFirst toUpper

emptyLine = Line "" [""]

codeGenDataTypeRef :: DataTypeInfoExpr -> String -> [Line]
codeGenDataTypeRef (DataTypeInfoExpr dName exprType) indent = 
    [Line indent [
        printf "public partial class %s" $ dataTypeName dName,
        "{"]] ++
    contents exprType ++
    [Line nextIndent [idLens]] ++
    [Line indent $[
        "}",
        "",
        printf "public static partial class %sOptics" $ dataTypeName dName,
        "{" ]] ++
    opticsExtensions exprType ++
    [Line indent [
        "}"]]
    where
        idLensParams = intercalate ", " $ (take 4 . repeat . dataTypeName) dName
        idLens = printf "public static AlgebraicDataTypes.Optics.Lens<%s> Id { get; } = AlgebraicDataTypes.Optics.Lens.Create<%s>(s => s, (a2b,s) => a2b(s));" idLensParams idLensParams 
        nextIndent = (head indent) : indent
        nextIndent2 = (head nextIndent) : nextIndent
        contents :: DataTypeExpr -> [Line]
        contents (ProductExpr dataRefs) = [
            Line nextIndent $ map declLine dataRefs,
            Line nextIndent [
                 printf "public %s(%s) => (%s) = (%s);" (dataTypeName dName) dParams1 declNamesCommaDelim dParams2
                ,""]]
            where 
                declLine d = "public " ++ d^.memberType^.to dataTypeName ++ " " ++ d^.memberName^.coerced^.to upperCamelCase ++ "{ get; }"
                dParams1 = intercalate ", " $ map (\dr -> dr^.memberType^.to dataTypeName ++ " " ++ dr^.memberName^.coerced^.to lowerCamelCase) dataRefs
                dParams2 = intercalate ", " $ map (\dr -> dr^.memberName^.coerced^.to lowerCamelCase) dataRefs
                declNamesCommaDelim = intercalate ", " $ map (\dr -> dr^.memberName^.coerced^.to upperCamelCase) dataRefs
        contents (SumExpr dataRefs) = concat $ intersperse [emptyLine] $ map derivedClass dataRefs
            where 
                derivedClass :: DataTypeRef -> [Line]
                derivedClass d = [
                    Line nextIndent [
                     "public virtual bool Is" ++ (d^.memberName^.coerced) ++ " { get => false; }",
                     "public static _" ++ (d^.memberName^.coerced) ++ " Create" ++ (d^.memberName^.coerced) ++ "(" ++ dataTypeName (d^.memberType^.coerced) ++ " val) => new _" ++ (d^.memberName^.coerced) ++ "(val);",
                     "public partial class _" ++ (d^.memberName^.coerced) ++ " : " ++ dataTypeName dName,
                     "{"],
                    Line nextIndent2 [
                        "public " ++ dataTypeName (d^.memberType) ++ " Value { get; }",
                        "public _" ++ (d^.memberName^.coerced) ++ "(" ++ dataTypeName (d^.memberType) ++ " value) => Value = value;",
                        "public override bool Is" ++ (d^.memberName^.coerced) ++ " { get => true; }"],
                    Line nextIndent [
                     "}"]]  
        opticsExtensions :: DataTypeExpr -> [Line]
        opticsExtensions (ProductExpr dataRefs) = [Line nextIndent $ concat $ intersperse [""] $ map memberOptics dataRefs]
            where 
                mName dr = dr^.memberName^.coerced^.to upperCamelCase
                lensName x = mName x ++ "Lens"
                s = dataTypeName dName
                typeFor d = d^.memberType^.to dataTypeName
                altConstructor d = "new " ++ s ++ "(" ++ intercalate ", " (map (\x -> if x == d then "s2a(s." ++ mName x ++ ")" else "s." ++ mName x) dataRefs) ++ ")"
                memberOptics d = [
                    "public static Lens<" ++ s ++ ", " ++ s ++ ", " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ lensName d ++ " = Lens.Create<" ++ s ++ ", " ++ s ++ ", " ++ typeFor d ++ ", " ++ typeFor d ++ ">(s => s." ++ mName d ++ ", (s2a, s) => " ++ altConstructor d ++ ");",
                    "public static ILens<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ILens<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");",
                    "public static ITraversal<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ITraversal<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");",
                    "public static ISetter<S,T, " ++ typeFor d ++ ", " ++ typeFor d ++ "> " ++ mName d ++ "<S,T>(this ISetter<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");",
                    "public static IGetter<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IGetter<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");",
                    "public static IFold<S, " ++ typeFor d ++ "> " ++ mName d ++ "<S>(this IFold<S, " ++ s ++ "> other) => other.ComposeWith(" ++ lensName d ++ ");"]
        opticsExtensions (SumExpr dataTypeRefs) = concat $ intersperse [emptyLine] $ map memberPrism dataTypeRefs
            where 
                prismName d = (d^.memberName^.coerced) ++ "Prism"
                s = dataTypeName dName
                eitherName d = "Either<" ++ dataTypeName dName ++ ", " ++ dataTypeName (d^.memberType) ++ ">"
                memberPrism d = [
                    Line nextIndent  ["public static Prism<" ++ s ++ ", " ++ s ++ ", " ++ dataTypeName (d^.memberType) ++ ", " ++ dataTypeName (d^.memberType) ++ "> " ++ prismName d ++ " = Prism.Create<" ++ s ++ ", " ++ s ++ ", " ++ dataTypeName (d^.memberType) ++ ", " ++ dataTypeName (d^.memberType) ++ ">("],
                    Line nextIndent2 ["b => " ++ s ++ ".Create" ++ (d^.memberName^.coerced) ++ "(b),",
                                      "s => { if (s is " ++ s ++ "._" ++ (d^.memberName^.coerced) ++ " a) return new " ++ eitherName d ++ ".Right(a.Value); else return new " ++ eitherName d ++ ".Left(s); });"],
                    Line nextIndent  ["public static IPrism<S,T, " ++ dataTypeName (d^.memberType) ++ ", " ++ dataTypeName (d^.memberType) ++ "> " ++ (d^.memberName^.coerced) ++ "<S,T>(this IPrism<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ prismName d ++ ");",
                                      "public static ITraversal<S,T, " ++ dataTypeName (d^.memberType) ++ ", " ++ dataTypeName (d^.memberType) ++ "> " ++ (d^.memberName^.coerced) ++ "<S,T>(this ITraversal<S,T, " ++ s ++ ", " ++ s ++ "> other) => other.ComposeWith(" ++ prismName d ++ ");"]]

importNamespaces :: [Namespace] -> Line
importNamespaces n = Line "" $ map (\ns -> "using " ++ ns^.coerced ++ ";") n
        
getCodeGenTreeLines :: CodeGenTree -> [Line]
getCodeGenTreeLines tree = [
    tree^.imports^.to importNamespaces,
    Line "" [
        "namespace " ++ tree^.namespace^.coerced,
        "{"]] ++
    classes ++
    [Line "" ["}"]]
    where
        classes = concat $ map (\x -> codeGenDataTypeRef x "\t") $ tree^.dataTypes

linesToString :: [Line] -> String
linesToString ls = intercalate "\n" $ concat $ map lineToString ls where 
    lineToString :: Line -> [String]
    lineToString l = map (l^.indent ++) $ l^.text

evalCodeGenTree :: CodeGenTree -> String
evalCodeGenTree = linesToString . getCodeGenTreeLines 