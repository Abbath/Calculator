{-# LANGUAGE OverloadedStrings #-}
module Main where

import Calculator
import Calculator.Tests
import Options.Applicative
import Control.Monad ( when )
import Data.Char ( toLower )

data Options = Options {
      frontend :: !String,
      backend  :: !String,
      test     :: !Bool,
      port     :: !Int
    }

options :: Parser Options
options = Options
          <$> strOption (long "frontend" <> short 'f' <> help "Frontend (I, M, A)" <> metavar "FRONTEND" <> value "I")
          <*> strOption (long "backend" <> short 'b' <> help "Backend (C, W, T, TS)" <> metavar "BACKEND" <> value "C")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> option auto (long "port" <> short 'p' <> help "Port" <> metavar "PORT" <> value 3000)

main :: IO ()
main = do
  writeFile "ids" "[]"
  opts2 <- execParser opts
  when (test opts2) testLoop
  let f = selectFront $ frontend opts2
  case map toLower $ backend opts2 of
    "c"  -> evalLoop f
    "w"  -> webLoop (port opts2) f
    "t"  -> telegramLoop f
    "ts" -> telegramSimple f
    _    -> webLoop (port opts2) f
  where opts = info (helper <*> options)
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
        selectFront s = case map toLower s of
          "i" -> Internal
          "m" -> Megaparsec
          "a" -> AlexHappy
          _ -> Internal

