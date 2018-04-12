{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Process (callCommand)
import System.Directory (createDirectoryIfMissing)
import Text.PDF.Info (pdfInfo, pdfInfoPages)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess, ExitFailure))
import Data.List (isSuffixOf)

main = getArgs >>= parse >>= svgToTex

svgToTex :: FilePath -> IO ()
svgToTex fileName = do
  let tmpDir = "tmp"
  let pdfFile = tmpDir ++ "/" ++ fileName ++ ".pdf"
  let pdfTexFile = tmpDir ++ "/" ++ fileName ++ ".pdf_tex"
  let svgFile = fileName ++ ".svg"
  createDirectoryIfMissing True tmpDir
  createPDF svgFile pdfFile
  maxPage <- getMaxPage pdfFile
  let badPageString = "page=" ++ show (maxPage + 1)
  let goodPageString = "page=" ++ show maxPage
  replaceStrInFile pdfTexFile badPageString goodPageString

parse :: [String] -> IO String
parse ["-h"] = usage >> exit
parse [file]
  | isSuffixOf ext file = return $ take (length file - length ext) file
  | otherwise = return file
  where ext = ".svg"
parse _ = usage >> exit
 
usage = putStrLn "Usage: svg_to_tex [-h] FILENAME"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

createPDF :: FilePath -> FilePath -> IO ()
createPDF svgFile pdfFile = callCommand inkscapeCommand
  where
    inkscapeCommand = "inkscape -z"
      ++ " --file=" ++ svgFile
      ++ " --export-pdf=" ++ pdfFile
      ++ " --export-latex"

getMaxPage :: FilePath -> IO (Integer)
getMaxPage pdf = do
  x <- pdfInfo pdf
  pagesFromInfo x
  where
    pagesFromInfo (Left _) = error "pdfinfo failed: Could not get info"
    pagesFromInfo (Right info) = case pdfInfoPages info of
      (Just pages) -> return pages
      Nothing -> error "pdfinfo failed: No pages found"

replaceStrInFile :: FilePath -> String -> String -> IO ()
replaceStrInFile fileName needle replacement = do
  txt <- T.readFile fileName
  T.writeFile fileName $ T.replace (T.pack needle) (T.pack replacement) txt

-- fixOutput :: FilePath -> IO ()
-- fixOutput = _
