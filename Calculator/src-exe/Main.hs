{-# LANGUAGE OverloadedStrings, CPP, MultiWayIf #-}
module Main where

import Calculator
    ( CompileMode(CompRead, CompLoad, CompStore),
      Mode(Internal),
      compileAndRunFile,
      evalLoop,
      evalFile,
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
      Parser )
import Data.Char ( toLower )
import Paths_Calculator (version)
import Data.Version (showVersion)
#ifdef DISCORD
import Calculator.Opts.Dis ( discordCalculator )
#endif
#ifdef RAYLIB
import Calculator.Opts.Ray ( raylibLoop )
#endif
#ifdef TELEGRAM
import Calculator.Opts.Tel
#endif

data Options = Options {
      input       :: !FilePath,
      source      :: !FilePath,
      compileMode :: !String,
      frontend    :: !String,
      test        :: !Bool,
      raylib      :: !Bool,
      discord     :: !Bool,
      ver     :: !Bool,
      port        :: !Int
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

main :: IO ()
main = do
  writeFile "ids" "[]"
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
    | not . null . input $ opts2 -> evalFile (input opts2)
    | not . null . source $ opts2 -> compileAndRunFile (source opts2) $ case compileMode opts2 of
        "L" -> CompLoad
        "S" -> CompStore
        "R" -> CompRead
        _ -> CompRead
    | otherwise -> do
      case map toLower $ frontend opts2 of
        "c"  -> evalLoop Internal
        "w"  -> webLoop (port opts2) Internal
#ifdef TELEGRAM
        "t"  -> telegramSimple Internal
#endif
        _    -> webLoop (port opts2) Internal
      where opts = info (helper <*> options)
              ( fullDesc
                <> progDesc "Reads a character string and prints the result of calculation"
                <> header "Calculator - a simple string calculator" )
