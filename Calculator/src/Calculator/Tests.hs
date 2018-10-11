module Calculator.Tests (testLoop) where

import           Calculator.Evaluator
import           Calculator.Lexer
import qualified Calculator.MegaParser as CMP
import           Calculator.Parser
import           Calculator.Types      (Assoc (..), Expr (..))
import           Control.Lens          ((%~), (&), (^.), _1, _3)
import           Control.Monad.Reader
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import qualified Text.Megaparsec       as MP
import           Control.Arrow         ((***))

data Backend = Internal | Mega deriving Show

type Tests = [(String, Either String Rational)]

opMap :: OpMap
opMap = M.fromList [("=", f 0 R)
  , ("==", f 1 L), ("<=", f 1 L), (">=", f 1 L), ("!=", f 1 L), ("<", f 1 L), (">", f 1 L)
  , ("+", f 2 L), ("-", f 2 L)
  , ("*", f 3 L), ("/", f 3 L), ("%", f 3 L)
  , ("^", f 4 R)]
  where f p a = ((p, a), Number 0)

getPrA :: OpMap -> Map String (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (id *** fst ) lst
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
              Right r  -> eval maps r
    let tt = either (Left . fst) (Right . fst) t          
    do
      putStrLn $ if tt == snd x
        then "Passed: " ++ sample
        else "Failed: " ++ sample ++ " expected: " ++ show x ++ " received: " ++ show t
      loop xs (case t of
        Right (r,m)  -> m & _1 %~ M.insert "_" r
        Left (_,m) -> m) bk
    
  else do
    putStrLn "Empty!"
    loop xs maps bk

errorToEither :: Show a => Either a b -> Either String b
errorToEither (Left err) = Left (show err)
errorToEither (Right r)  = Right r

tests :: Tests
tests = [
   ("_", Right 0)
  ,("1", Right 1)
  ,("_", Right 1)
  ,("-1", Right (-1))
  ,("2+2", Right 4)
  ,("2-2-2-2", Right (-4))
  ,("1024/2/2/2", Right 128)
  ,("f(x) = x", Left "Function f/1")
  ,("f(g,x) = g(x)", Left "Function f/2")
  ,("f(sin,pi)", Right 0)
  ,("p(x,y) = x - y", Left "Function p/2")
  ,("p(2,2)", Right 0)
  ,("&(1,0) = x - y", Left "Operator & p=1 a=left")
  ,("2&2&2&2", Right (-4))
  ,("2&2-2&2", Right 0)
  ,("&(2,0) = x - y", Left "Operator & p=2 a=left")
  ,("2&2-2&2", Right (-4))
  ,("2^3^4", Right 2417851639229258349412352)
  ,("2+2*2", Right 6)
  ,("-((1))", Right (-1))
  ,("-1^2", Right 1)
  ,("(2+2)*2", Right 8)
  ,("x = 5",Left "Constant x=5")
  ,("abs(-x)==x", Right 1)
  ,("1!=2", Right 1)
  ,("sin(pi)==0", Right 1)
  ,("/= = !=", Left "Operator alias /= = !=")
  ]

defVar :: VarMap
defVar = M.fromList [("pi", toRational (pi :: Double)), ("e", toRational . exp $ (1.0 :: Double)), ("_",0.0)]

testLoop :: IO ()
testLoop = do
  loop tests (defVar, M.empty, opMap) Internal
  loop tests (defVar, M.empty, opMap) Mega