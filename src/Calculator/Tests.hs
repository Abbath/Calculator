module Calculator.Tests (testLoop) where

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

data Backend = Internal | Mega deriving Show

type Tests = [(String, Rational)]

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

loop :: Tests -> Maps -> Backend -> IO ()
loop [] _ _ = return ()
loop (x:xs) maps bk = do
  let sample = fst x
  if not $ null sample
  then do
    let e = case bk of
              Internal -> tokenize sample >>= parse (getPriorities $ maps^._3)
              Mega -> errorToEither (MP.runParser (runReaderT CMP.parser (getPrA $ maps^._3)) "" (sample++"\n"))
    --print e
    let t = case e of 
              Left err -> Left (err, maps) 
              Right r -> eval maps r
    case t of
      Right (r,m) -> do
        putStrLn $ if r == snd x
        then "Passed: " ++ sample
        else "Failed: " ++ sample ++ " expected: " ++ show (snd x) ++ " received: " ++ show t
        loop xs (m & _1 %~ M.insert "_" r) bk
      Left (err,_) -> do
        putStrLn err
        print maps
        loop xs maps bk
  else do
    putStrLn "Empty!"
    loop xs maps bk


errorToEither :: Either MP.ParseError Expr -> Either String Expr
errorToEither (Left err) = Left (show err)
errorToEither (Right r) = Right r

tests :: Tests
tests = [
   ("_", 0)
  ,("1", 1)
  ,("_", 1)
  ,("-1", -1)
  ,("2+2", 4)
  ,("2-2-2-2", -4)
  ,("1024/2/2/2", 128)
  ,("f(x) = x", 0)
  ,("f(g,x) = g(x)", 1)
  ,("f(sin,pi)", 0)
  ,("p(x,y) = x - y", 2)
  ,("p(2,2)", 0)
  ,("&(1,0) = x - y", 13)
  ,("2&2&2&2", -4)
  ,("2&2-2&2", 0)
  ,("&(2,0) = x - y", 14)
  ,("2&2-2&2", -4)
  ,("2^3^4", 2417851639229258349412352)
  ,("2+2*2", 6)
  ,("-((1))", -1)
  ,("-1^2", 1)
  ,("(2+2)*2", 8)
  ,("x = 5",5)
  ,("abs(-x)==x", 1)
  ,("1!=2", 1)
  ,("sin(pi)==0", 1)
  ,("/= = !=", 14)
  ]

defVar :: VarMap
defVar = M.fromList [("pi", toRational (pi :: Double)), ("e", toRational . exp $ (1.0 :: Double)), ("_",0.0)]

testLoop :: IO ()
testLoop = do
  loop tests (defVar, M.empty, opMap) Internal
  loop tests (defVar, M.empty, opMap) Mega
