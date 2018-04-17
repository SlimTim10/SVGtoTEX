{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process (callCommand)
import System.Directory (createDirectoryIfMissing)
import Text.PDF.Info (pdfInfo, pdfInfoPages)
import Options.Applicative (execParser)

import Lib (replaceStrInFile, stripExt)
import CommandOptions
  (opts
  , Options
    ( Options
    , optFileName
    , optInDir
    , optOutDir
    )
  )

main :: IO ()
main = do
  options <- execParser opts
  svgToTex options

svgToTex :: Options -> IO ()
svgToTex (Options { optFileName=fileName, optInDir=inDir, optOutDir=outDir } ) = do
  let file = stripExt ".svg" fileName
  let pdfFile = outDir ++ "/" ++ file ++ ".pdf"
  let pdfTexFile = outDir ++ "/" ++ file ++ ".pdf_tex"
  let svgFile = inDir ++ "/" ++ file ++ ".svg"
  createDirectoryIfMissing True outDir
  createPDF svgFile pdfFile
  maxPage <- getMaxPage pdfFile
  let badPageString = "page=" ++ show (maxPage + 1)
  let goodPageString = "page=" ++ show maxPage
  replaceStrInFile pdfTexFile badPageString goodPageString

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
