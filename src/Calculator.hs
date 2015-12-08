module Calculator (evalLoop) where

import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Exit (exitSuccess, exitFailure)
import Data.Map (Map)
import qualified Data.Map as M
import Calculator.Types
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

loop :: (VarMap, FunMap) -> IO()
loop (m,mm) = do
  putStr "> " >> hFlush stdout
  x <- getLine `catchIOError` (\e -> if isEOFError e
    then do {putStrLn "\nBye!"; exitSuccess}
    else do {print e; exitFailure})
  if not $ null x
  then do
    let t = tokenize x >>= parse >>= eval (m, mm)
    case t of
      Left err -> do
        putStrLn err
        loop (m,mm)
      Right (r,(m1,m2)) -> do
        print r
        loop (M.insert "_" r m1,m2)
  else do
    putStrLn "Empty!"
    loop (m,mm)

defVar :: Map String Double
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_", 0.0)]

evalLoop = loop (defVar, M.fromList [])