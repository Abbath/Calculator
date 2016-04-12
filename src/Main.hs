module Main where

import Calculator
import Calculator.Tests
import Options.Applicative

data Options = Options {
      mp :: Bool,
      test :: Bool,
      cli :: Bool
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "cli" <> short 'c' <> help "Run console app")
    
main :: IO ()
main = do
  Options m t c <- execParser opts
  case (m,t,c) of
    (_,True,_) -> testLoop
    (True,_,False) -> webLoop Megaparsec
    (False,_,False) -> webLoop Internal
    (True,_,True) -> evalLoop Megaparsec
    (False,_,True) -> evalLoop Internal    
  where opts = info (helper <*> options) 
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
