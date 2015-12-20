module Calculator (evalLoop) where

import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, isEOFError)
import System.Exit (exitSuccess, exitFailure)
import Data.Map.Strict (Map)
import Control.Lens (_1, _3, (^.), (&), (%~))
import qualified Data.Map.Strict as M
import Calculator.Types (Expr(..), Assoc(..))
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator
import qualified Text.Megaparsec as MP
import qualified Calculator.MegaParser as CMP
import System.Environment (getArgs)
import Control.Monad.Reader

opMap :: OpMap
opMap = M.fromList [("=", f 0 R)
  , ("==", f 1 L), ("<=", f 1 L), (">=", f 1 L), ("!=", f 1 L), ("<", f 1 L), (">", f 1 L)
  , ("+", f 2 L), ("-", f 2 L)
  , ("*", f 3 L), ("/", f 3 L), ("%", f 3 L)
  , ("^", f 4 R)]
  where f p a = ((p, a), Number 0)

getPrA :: OpMap -> Map String (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (\(s,(pa,_)) -> (s,pa)) lst
            in ps

loop :: Maps -> IO ()
loop maps = do
  putStr "> " >> hFlush stdout
  args <- getArgs
  x <- getLine `catchIOError` (\e -> if isEOFError e
    then do {putStrLn "\nBye!"; exitSuccess}
    else do {print e; exitFailure})
  if not $ null x
  then
    case args of
      ["-mp"] -> megaLoop x maps
      _ -> do
        let t = tokenize x >>= parse (getPriorities $ maps^._3) >>= eval maps
        internalLoop t maps
  else do
    putStrLn "Empty!"
    loop maps

megaLoop :: String -> Maps -> IO ()
megaLoop x maps = do
  let p = MP.runParser (runReaderT CMP.parser (getPrA $ maps^._3)) "" (x++"\n")
  case p of
    Left err -> do
      print err
      loop maps
    Right s -> do
      let t = eval maps s
      internalLoop t maps

internalLoop :: Either String (Double, Maps) -> Maps -> IO ()
internalLoop t maps =
  case t of
    Left err -> do
      putStrLn err
      loop maps
    Right (r, m) -> do
      print r
      loop $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_",0.0)]

evalLoop :: IO ()
evalLoop = loop (defVar, M.empty, opMap)