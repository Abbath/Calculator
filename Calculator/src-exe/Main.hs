{-# LANGUAGE OverloadedStrings #-}
module Main where

import Calculator
import Calculator.Tests ( testLoop )
import Options.Applicative
import Control.Monad ( when, unless )
import Data.Char ( toLower )

data Options = Options {
      input    :: !FilePath,
      frontend :: !String,
      backend  :: !String,
      test     :: !Bool,
      port     :: !Int
    }

options :: Parser Options
options = Options
          <$> strOption (long "input" <> short 'i' <> help "Input file" <> metavar "FILENAME" <> value "")
          <*> strOption (long "frontend" <> short 'f' <> help "Frontend (C, W, T)" <> metavar "FRONTEND" <> value "C")
          <*> strOption (long "backend" <> short 'b' <> help "Backend (I, M, A)" <> metavar "BACKEND" <> value "I")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> option auto (long "port" <> short 'p' <> help "Port" <> metavar "PORT" <> value 3000)

main :: IO ()
main = do
  writeFile "ids" "[]"
  opts2 <- execParser opts
  when (test opts2) testLoop
  unless (null . input $ opts2) $ evalFile (input opts2)
  let f = selectBack $ backend opts2
  case map toLower $ frontend opts2 of
    "c"  -> evalLoop f
    "w"  -> webLoop (port opts2) f
    "t"  -> telegramSimple f
    _    -> webLoop (port opts2) f
  where opts = info (helper <*> options)
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
        selectBack s = case map toLower s of
          "i" -> Internal
          "m" -> Megaparsec
          "a" -> AlexHappy
          _ -> Internal
