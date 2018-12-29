module Main where

import Lib
import DataTypes
import Parser
import CodeGen
import Control.Applicative
import Text.Trifecta
import System.Environment (getArgs)

lumpy :: String -> IO ()
lumpy arg = do
  r <- parseFromFile parseDataFile arg
  case r of
    Nothing -> return ()
    Just rs -> print rs

main :: IO ()
main = mapM_ lumpy =<< getArgs