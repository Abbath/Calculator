module Calculator (Mode(..), evalLoop) where

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
import System.Console.Haskeline
import Control.Arrow (left)

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
loop mode maps = runInputT (defaultSettings { historyFile = Just "/home/dan/.mycalchist", autoAddHistory = True}) (loop' mode maps)
  where 
  loop' :: Mode -> Maps -> InputT IO ()
  loop' md ms = do
    input <- getInputLine "> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just x -> do
        let t = case md of
                  Megaparsec -> 
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (x++"\n")) >>= eval ms
                  Internal -> 
                    tokenize x >>= parse (getPriorities $ ms^._3) >>= eval ms
        case t of 
          Left err -> do
            liftIO $ putStrLn err
            loop' md ms
          Right (r, m) -> do
            liftIO $ print r
            loop' md $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_",0.0)]

evalLoop :: Mode -> IO ()
evalLoop m = loop m (defVar, M.empty, opMap)
