{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Calculator
import           Calculator.Tests
import           Options.Applicative

data Options = Options {
      mp   :: !Bool,
      ah   :: !Bool,
      test :: !Bool,
      cli  :: !Bool,
      tg   :: !Bool,
      tgs  :: !Bool,
      port :: ![Int]
    }

options :: Parser Options
options = Options
          <$> switch (long "megaparsec-frontend" <> short 'm' <> help "Use Megaparsec frontend")
          <*> switch (long "alex-happy-frontend" <> short 'x' <> help "Use Alex+Happy frontend (very experimental)")
          <*> switch (long "test" <> short 't' <> help "Run tests")
          <*> switch (long "cli" <> short 'c' <> help "Run console app")
          <*> switch (long "telegram" <> short 'g' <> help "Run Telegram bot")
          <*> switch (long "telegram-simple" <> short 's' <> help "Run simple Telegram bot (only Internal frontend supported)")
          <*> many (argument auto (metavar "PORT"))

main :: IO ()
main = do
  writeFile "ids" "[]"
  -- Options m x t c g s p <- execParser opts
  opts2 <- execParser opts
  let p = port opts2
  case opts2 of
    Options _ _ True _ _ _ _          -> testLoop
    Options True _ _ _ _ True _       -> telegramSimple Megaparsec
    Options _ True _ _ _ True _       -> telegramSimple AlexHappy
    Options False False _ _ _ True _  -> telegramSimple Internal
    Options True _ _ _ True _ _       -> telegramLoop Megaparsec
    Options _ True _ _ True _ _       -> telegramLoop AlexHappy
    Options False False _ _ True _ _  -> telegramLoop Internal
    Options True _ _ False _ _ _      -> webLoop (portCheck p) Megaparsec
    Options _ True _ False _ _ _      -> webLoop (portCheck p) AlexHappy
    Options False False _ False _ _ _ -> webLoop (portCheck p) Internal
    Options True _ _ True _ _ _       -> evalLoop Megaparsec
    Options _ True _ True _ _ _       -> evalLoop AlexHappy
    Options False False _ True _ _ _  -> evalLoop Internal
  where opts = info (helper <*> options)
          ( fullDesc
            <> progDesc "Reads a character string and prints the result of calculation"
            <> header "Calculator - a simple string calculator" )
        portCheck [p]   = p
        portCheck []    = 3000
        portCheck [_,_] = error "Yup. Its an Easter egg!"
        portCheck _     = error "There are too many args!"

