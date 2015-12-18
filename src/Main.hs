module Main where

import Calculator
import Calculator.Tests

main :: IO ()
main = do
  testLoop
  evalLoop
