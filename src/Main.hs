module Main where

import Calculator
import Calculator.Tests
import Options.Applicative

data Options = Options {
      mp :: Bool,
      test :: Bool,
      web :: Bool
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "web" <> short 'w' <> help "Run on localhost 3000")
    
main :: IO ()
main = do
  Options m t w <- execParser opts
  case (m,t,w) of
    (_,True,_) -> testLoop
    (True,_,False) -> evalLoop Megaparsec
    (False,_,False) -> evalLoop Internal
    (True,_,True) -> webLoop Megaparsec
    (False,_,True) -> webLoop Internal    
  where opts = info (helper <*> options) 
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
