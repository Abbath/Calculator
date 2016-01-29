module Calculator (Mode(..), evalLoop) where

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
import Control.Monad.Reader

data Mode = Internal | Megaparsec deriving Show

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

loop :: Mode -> Maps -> IO ()
loop mode maps = do
  putStr "> " >> hFlush stdout
  x <- getLine `catchIOError` (\e -> if isEOFError e
    then do {putStrLn "\nBye!"; exitSuccess}
    else do {print e; exitFailure})
  if not $ null x
  then
    case mode of
      Megaparsec -> megaLoop mode x maps
      Internal -> do
        let t = tokenize x >>= parse (getPriorities $ maps^._3) >>= eval maps
        internalLoop mode t maps
  else do
    putStrLn "Empty!"
    loop mode maps

megaLoop :: Mode -> String -> Maps -> IO ()
megaLoop mode x maps = do
  let p = MP.runParser (runReaderT CMP.parser (getPrA $ maps^._3)) "" (x++"\n")
  case p of
    Left err -> do
      print err
      loop mode maps
    Right s -> do
      let t = eval maps s
      internalLoop mode t maps

internalLoop :: Mode -> Either String (Double, Maps) -> Maps -> IO ()
internalLoop mode t maps =
  case t of
    Left err -> do
      putStrLn err
      loop mode maps
    Right (r, m) -> do
      print r
      loop mode $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_",0.0)]

evalLoop :: Mode -> IO ()
evalLoop m = loop m (defVar, M.empty, opMap)
