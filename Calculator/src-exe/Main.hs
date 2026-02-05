{-# LANGUAGE OverloadedStrings, CPP, MultiWayIf #-}
module Main where

import Calculator
    ( CompileMode(CompRead, CompLoad, CompStore),
      compileAndRunFile,
      evalLoop,
      evalFile,
      evalString,
      webLoop )
import Calculator.Tests ( testLoop )
import Options.Applicative
    ( auto,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      strOption,
      switch,
      value,
      execParser,
      helper,
      Parser, 
      optional)
import Control.Monad (void)
import Data.Char ( toLower )
import Paths_Calculator (version)
import Data.Version (showVersion)
import Data.Maybe (fromMaybe, isJust)
#ifdef DISCORD
import Calculator.Opts.Dis ( discordCalculator )
#endif
#ifdef RAYLIB
import Calculator.Opts.Ray ( raylibLoop )
#endif
#ifdef TELEGRAM
import Calculator.Opts.Tel
#endif
import System.Directory (getXdgDirectory, XdgDirectory (..), createDirectoryIfMissing )

data Options = Options {
      input       :: !FilePath,
      source      :: !FilePath,
      compileMode :: !String,
      frontend    :: !String,
      test        :: !Bool,
      raylib      :: !Bool,
      discord     :: !Bool,
      ver         :: !Bool,
      port        :: !Int,
      expr        :: !(Maybe String)
    }

options :: Parser Options
options = Options
          <$> strOption (long "input" <> short 'i' <> help "Input file" <> metavar "FILENAME" <> value "")
          <*> strOption (long "source" <> short 's' <> help "Source file" <> metavar "SOURCE" <> value "")
          <*> strOption (long "compile-mode" <> short 'c' <> help "Compile mode (S, L, R)" <> metavar "COMPILE_MODE" <> value "R")
          <*> strOption (long "frontend" <> short 'f' <> help "Frontend (C, W, T)" <> metavar "FRONTEND" <> value "C")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "raylib" <> short 'r' <> help "Raylib GUI")
          <*> switch (long "discord" <> short 'd' <> help "Discord bot")
          <*> switch (long "version" <> short 'v' <> help "Version")
          <*> option auto (long "port" <> short 'p' <> help "Port" <> metavar "PORT" <> value 3000)
          <*> optional (strOption (long "expression" <> short 'e' <> help "Evaluate expression" <> metavar "EXPRESSION"))

main :: IO ()
main = do
  hd <- getXdgDirectory XdgCache "Calculator"
  createDirectoryIfMissing False hd
  writeFile (hd <> "/ids") "[]"
  opts2 <- execParser opts
  if
    | test opts2 -> testLoop
    | ver opts2 -> putStrLn $ showVersion version
#ifdef RAYLIB
    | raylib opts2 -> raylibLoop
#endif
#ifdef DISCORD
    | discord opts2 -> discordCalculator
#endif
    | isJust (expr opts2) -> void $ evalString (fromMaybe "" $ expr opts2) 
    | not . null . input $ opts2 -> void $ evalFile (input opts2)
    | not . null . source $ opts2 -> compileAndRunFile (source opts2) $ case compileMode opts2 of
        "L" -> CompLoad
        "S" -> CompStore
        "R" -> CompRead
        _ -> CompRead
    | otherwise -> do
      case map toLower $ frontend opts2 of
        "c"  -> evalLoop 
        "w"  -> webLoop (port opts2) 
#ifdef TELEGRAM
        "t"  -> telegramSimple 
#endif
        _    -> webLoop (port opts2) 
      where opts = info (helper <*> options)
              ( fullDesc
                <> progDesc "Reads a character string and prints the result of calculation"
                <> header "Calculator - a simple string calculator" )
