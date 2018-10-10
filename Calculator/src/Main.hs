{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Calculator
import           Calculator.Tests
import           Options.Applicative

data Options = Options {
      mp   :: Bool,
      ah   :: Bool,
      test :: Bool,
      cli  :: Bool,
      tg   :: Bool,
      port :: [Int]
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-backend" <> short 'm' <> help "Use Megaparsec backend")
          <*> switch (long "alex-happy-backend" <> short 'x' <> help "Use Alex+Happy backend (very experimental)")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "cli" <> short 'c' <> help "Run console app")
          <*> switch (long "telegram" <> short 'g' <> help "Run Telegram bot")
          <*> many (argument auto (metavar "PORT"))

main :: IO ()
main = do
  writeFile "ids" "[]"
  Options m x t c g p <- execParser opts
  case (m,x,t,c,g) of
    (_,_,True,_,_)          -> testLoop
    (True,_,_,_,True)       -> telegramLoop Megaparsec
    (_,True,_,_,True)       -> telegramLoop AlexHappy
    (False,False,_,_,True)  -> telegramLoop Internal
    (True,_,_,False,_)      -> webLoop (portCheck p) Megaparsec
    (_,True,_,False,_)      -> webLoop (portCheck p) AlexHappy
    (False,False,_,False,_) -> webLoop (portCheck p) Internal
    (True,_,_,True,_)       -> evalLoop Megaparsec
    (_,True,_,True,_)       -> evalLoop AlexHappy
    (False,False,_,True,_)  -> evalLoop Internal
  where opts = info (helper <*> options)
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
        portCheck [p]   = p
        portCheck []    = 3000
        portCheck [_,_] = error "Yup. Its an Easter egg!"
        portCheck _     = error "There are too many args!"
