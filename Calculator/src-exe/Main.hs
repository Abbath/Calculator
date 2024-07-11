{-# LANGUAGE OverloadedStrings, CPP, MultiWayIf #-}
module Main where

import Calculator
import Calculator.Tests ( testLoop )
import Options.Applicative
import Control.Monad ( when )
import Data.Char ( toLower )

data Options = Options {
      input       :: !FilePath,
      source      :: !FilePath,
      compileMode :: !String,
      frontend    :: !String,
      backend     :: !String,
      test        :: !Bool,
      raylib      :: !Bool,
      discord     :: !Bool,
      port        :: !Int
    }

options :: Parser Options
options = Options
          <$> strOption (long "input" <> short 'i' <> help "Input file" <> metavar "FILENAME" <> value "")
          <*> strOption (long "source" <> short 's' <> help "Source file" <> metavar "SOURCE" <> value "")
          <*> strOption (long "compile-mode" <> short 'c' <> help "Compile mode (S, L, R)" <> metavar "COMPILE_MODE" <> value "R")
          <*> strOption (long "frontend" <> short 'f' <> help "Frontend (C, W, T)" <> metavar "FRONTEND" <> value "C")
          <*> strOption (long "backend" <> short 'b' <> help "Backend (I)" <> metavar "BACKEND" <> value "I")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "raylib" <> short 'r' <> help "Raylib GUI")
          <*> switch (long "discord" <> short 'd' <> help "Discord bot")
          <*> option auto (long "port" <> short 'p' <> help "Port" <> metavar "PORT" <> value 3000)

main :: IO ()
main = do
  writeFile "ids" "[]"
  opts2 <- execParser opts
  when (test opts2) testLoop
  if
#ifdef RAYLIB
    | raylib opts2 -> raylibLoop
#endif
#ifdef DISCORD
    | discord opts2 -> pingpongExample
#endif
    | (not . null . input $ opts2) -> evalFile (input opts2)
    | (not . null . source $ opts2) -> compileAndRunFile (source opts2) $ case compileMode opts2 of
        "L" -> CompLoad
        "S" -> CompStore
        "R" -> CompRead
        _ -> CompRead
    | otherwise -> do
      let f = selectBack $ backend opts2
      case map toLower $ frontend opts2 of
        "c"  -> evalLoop f
        "w"  -> webLoop (port opts2) f
#ifdef TELEGRAM
        "t"  -> telegramSimple f
#endif
        _    -> webLoop (port opts2) f
      where opts = info (helper <*> options)
              ( fullDesc
                <> progDesc "Reads a character string and prints the result of calculation"
                <> header "Calculator - a simple string calculator" )
            selectBack s = case map toLower s of
              "i" -> Internal
              _ -> Internal
