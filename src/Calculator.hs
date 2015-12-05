module Calculator (evalLoop) where

import System.IO (hFlush, stdout)
import Data.Map (Map)
import qualified Data.Map as M
import Calculator.Types
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator

loop :: (Map String Double,Map (String,Int) ([String],Expr)) -> IO()
loop (m,mm) = do
    putStr "> " >> hFlush stdout
    x <- getLine
    if not (null x)
    then do
        let t = tokenize x >>= parse >>= eval (m, mm)
        case t of
           Left err -> do
            putStrLn err
            loop (m,mm)
           Right (r,m1,m2) -> do
            print r
            loop (M.insert "_" r m1,m2)
    else do
        putStrLn "Empty!"
        loop (m,mm)

defVar :: Map String Double
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_", 0.0)]

evalLoop = loop (defVar, M.fromList [])