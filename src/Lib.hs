module Lib
  ( replaceStrInFile
  , stripExt
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (isSuffixOf)

replaceStrInFile :: FilePath -> String -> String -> IO ()
replaceStrInFile fileName needle replacement = do
  txt <- T.readFile fileName
  T.writeFile fileName $ T.replace (T.pack needle) (T.pack replacement) txt

stripExt :: String -> FilePath -> FilePath
stripExt ext fileName
  | isSuffixOf ext fileName = take (length fileName - length ext) fileName
  | otherwise = fileName
