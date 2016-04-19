{-# LANGUAGE OverloadedStrings #-}
module Main where

import Calculator
import Calculator.Tests
import Options.Applicative

data Options = Options {
      mp :: Bool,
      test :: Bool,
      cli :: Bool,
      tg :: Bool,
      port :: [Int]    
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "cli" <> short 'c' <> help "Run console app")
          <*> switch (long "telegram" <> short 'g' <> help "Run Telegram bot")
          <*> many (argument auto (metavar "PORT"))
    
main :: IO ()
main = do
  writeFile "ids" "[]"
  Options m t c g p <- execParser opts
  case (m,t,c,g) of
    (_,True,_,_) -> testLoop
    (True,_,_,True) -> telegramLoop Megaparsec
    (False,_,_,True) -> telegramLoop Internal
    (True,_,False,_) -> webLoop (portCheck p) Megaparsec
    (False,_,False,_) -> webLoop (portCheck p) Internal
    (True,_,True,_) -> evalLoop Megaparsec
    (False,_,True,_) -> evalLoop Internal    
  where opts = info (helper <*> options) 
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
        portCheck [p] = p
        portCheck [] = 3000
        portCheck [_,_] = error "Yup. Its an Easter egg!"
        portCheck _ = error "There are too many args!"