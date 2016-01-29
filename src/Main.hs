module Main where

import Calculator
import Calculator.Tests
import Options.Applicative

data Options = Options {
      mp :: Bool,
      test :: Bool
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "test" <> short 't' <> help "Run tests")
    
main :: IO ()
main = do
  Options m t <- execParser opts
  if t 
  then testLoop 
  else if m 
       then evalLoop Megaparsec
       else evalLoop Internal     
  where opts = info (helper <*> options) 
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
