module Calculator (evalLoop) where

import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Exit (exitSuccess, exitFailure)
import Data.Map.Strict (Map)
import Control.Lens
import Control.Lens.Tuple (_1,_2,_3)
import qualified Data.Map.Strict as M
import Calculator.Types (Expr(..), Token(..), Assoc(..))
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

opMap :: OpMap
opMap = M.fromList [("=", f 0 R)
  , ("==", f 1 L), ("<=", f 1 L), (">=", f 1 L), ("/=", f 1 L), ("<", f 1 L), (">", f 1 L)
  , ("+", f 2 L), ("-", f 2 L)
  , ("*", f 3 L), ("/", f 3 L), ("%", f 3 L)
  , ("^", f 4 R)]
  where f p a = ((p, a), Number 0)

loop :: Maps -> IO()
loop maps = do
  putStr "> " >> hFlush stdout
  x <- getLine `catchIOError` (\e -> if isEOFError e
    then do {putStrLn "\nBye!"; exitSuccess}
    else do {print e; exitFailure})
  if not $ null x
  then do
    let t = tokenize x >>= parse (getPriorities $ maps^._3) >>= eval maps
    case t of
      Left err -> do
        putStrLn err
        loop maps
      Right (r, m) -> do
        print r
        loop $ m & _1 %~ M.insert "_" r
  else do
    putStrLn "Empty!"
    loop maps

defVar :: VarMap
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_",0.0)]

evalLoop = loop (defVar, M.empty, opMap)