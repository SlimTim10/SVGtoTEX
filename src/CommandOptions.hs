module CommandOptions
  ( opts
  , Options
    ( Options
    , optFileName
    , optInDir
    , optOutDir
    )
  ) where

import Options.Applicative
  ( strArgument
  , strOption
  , help
  , metavar
  , short
  , long
  , info
  , progDesc
  , fullDesc
  , header
  , helper
  , (<**>)
  , Parser
  , ParserInfo
  )
import Data.Semigroup ((<>))

data Options = Options
  { optFileName :: FilePath
  , optInDir :: FilePath
  , optOutDir :: FilePath
  }

parseFileName :: Parser FilePath
parseFileName = strArgument
  $ metavar "FILENAME"
  <> help "Target SVG file (with or without .svg extension)"

parseInDir :: Parser FilePath
parseInDir = strOption
  $ short 'd'
  <> long "directory"
  <> metavar "INPUT-DIRECTORY"
  <> help "Input directory path"

parseOutDir :: Parser FilePath
parseOutDir = strOption
  $ short 'o'
  <> long "output"
  <> metavar "OUTPUT-DIRECTORY"
  <> help "Output directory path"

parseOptions :: Parser Options
parseOptions = Options
  <$> parseFileName
  <*> parseInDir
  <*> parseOutDir

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
  $ fullDesc
  <> progDesc "Convert SVG file to TEX"
  <> header "svg_to_tex"
