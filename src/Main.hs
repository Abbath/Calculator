module Main where

import Calculator
import Calculator.Tests
import Options.Applicative

data Options = Options {
      mp :: Bool,
      test :: Bool,
      cli :: Bool,
      port :: Int    
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "cli" <> short 'c' <> help "Run console app")
          <*> argument auto (metavar "PORT")
    
main :: IO ()
main = do
  writeFile "ids" "[]"
  Options m t c p <- execParser opts
  case (m,t,c) of
    (_,True,_) -> testLoop
    (True,_,False) -> webLoop p Megaparsec
    (False,_,False) -> webLoop p Internal
    (True,_,True) -> evalLoop Megaparsec
    (False,_,True) -> evalLoop Internal    
  where opts = info (helper <*> options) 
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
