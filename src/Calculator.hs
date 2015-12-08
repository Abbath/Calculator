module Calculator (evalLoop) where

import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Exit (exitSuccess, exitFailure)
import Data.Map (Map)
import qualified Data.Map as M
import Calculator.Types (Expr(..))
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

loop :: Maps -> IO()
loop maps = do
  putStr "> " >> hFlush stdout
  x <- getLine `catchIOError` (\e -> if isEOFError e
    then do {putStrLn "\nBye!"; exitSuccess}
    else do {print e; exitFailure})
  if not $ null x
  then do
    let t = tokenize x >>= parse >>= eval maps
    case t of
      Left err -> do
        putStrLn err
        loop maps
      Right (r, (m1, m2)) -> do
        print r
        loop (M.insert "_" r m1, m2)
  else do
    putStrLn "Empty!"
    loop maps

defVar :: VarMap
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_",0.0)]

evalLoop = loop (defVar, M.fromList [])