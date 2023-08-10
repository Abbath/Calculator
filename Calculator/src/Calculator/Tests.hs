{-# LANGUAGE OverloadedStrings, OverloadedLists  #-}
module Calculator.Tests (testLoop) where

import Calculator.AlexLexer (alexScanTokens)
import Calculator.Builtins (opMap, getPrecedences, getFakePrecedences)
import Calculator.Evaluator (evalS, MessageType(ErrMsg, MsgMsg))
import qualified Calculator.HappyParser as HP
import Calculator.HomebrewLexer (tloop)
import qualified Calculator.MegaParser as CMP
import Calculator.Parser (parse)
import Calculator.Types (getPrA, preprocess, showT, Maps, VarMap, EvalState(..))
import Control.Lens ((%~), (&), (^.), _1, _2, _3)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (runState)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO (putStrLn)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.Random (StdGen, initStdGen)
import qualified Text.Megaparsec as MP
import Data.Complex
import Data.Bifunctor

data Backend = Internal | Mega | AH deriving Show

type Tests = [(Text, Either MessageType (Complex Rational))]

loop :: Tests -> Maps -> StdGen -> Backend -> Int -> IO Int
loop [] _ _ _ n = return n
loop (x:xs) mps rgen bk n = do
  let sample = fst x
  if not $ T.null sample
  then do
    let e = case bk of
              Internal -> tloop sample >>= parse (getPrecedences (mps^._3) <> getFakePrecedences (mps^._2))
              Mega -> errorToEither (MP.runParser (runReaderT CMP.parser (getPrA $ mps^._3)) "" (sample <> "\n"))
              AH -> Right $ preprocess . HP.parse . alexScanTokens $ T.unpack sample
    --print e
    let t = case e of
              Left err -> (Left (ErrMsg err), EvalState mps rgen M.empty)
              Right r  -> runState (runExceptT (evalS r)) (EvalState mps rgen M.empty)
    let tt = fst t
    do
      new_n <- if tt == snd x
        then do
          TIO.putStrLn $ "Passed: " <> sample
          return 0
        else do
          TIO.putStrLn $ "Failed: " <> sample <> " expected: " <> showT x <> " received: " <> showT t
          return 1
      let (m, g) = case t of
                    (Right r, EvalState m_ g_ _)  -> (m_ & _1 %~ M.insert "_" r, g_)
                    (Left _, EvalState m_ g_ _) -> (m_, g_)
      loop xs m g bk (n + new_n)

  else do
    TIO.putStrLn "Empty!"
    loop xs mps rgen bk n

errorToEither :: Show a => Either a b -> Either Text b
errorToEither (Left err) = Left (showT err)
errorToEither (Right r)  = Right r

tests :: Tests
tests = map (Data.Bifunctor.second ((:+ 0) <$>)) [
   ("_", Right 0)
  ,("1", Right 1)
  ,("_", Right 1)
  ,("-1", Right (-1))
  ,("2+2", Right 4)
  ,("2-2-2-2", Right (-4))
  ,("1024/2/2/2", Right 128)
  ,("f(x) = x", Left . MsgMsg $ "Function f/1")
  ,("f(g,x) = g(x)", Left . MsgMsg $ "Function f/2")
  ,("f(sin,m.pi)", Right 0)
  ,("h() = 2", Left . MsgMsg $ "Function h/0")
  ,("hh = h()", Left . MsgMsg $ "Variable hh=2")
  ,("p(x,y) = x - y", Left . MsgMsg $ "Function p/2")
  ,("p(2,2)", Right 0)
  ,("$(1,0) = x - y", Left . MsgMsg $ "Operator $ p=1 a=left")
  ,("2$2$2$2", Right (-4))
  ,("2$2-2$2", Right 0)
  ,("$(5,0) = x - y", Left . MsgMsg $ "Operator $ p=5 a=left")
  ,("2$2-2$2", Right (-4))
  ,("2^3^4", Right 2417851639229258349412352)
  ,("2+2*2", Right 6)
  ,("-((1))", Right (-1))
  ,("-1^2", Right 1)
  ,("(2+2)*2", Right 8)
  ,("x = 5",Left . MsgMsg $ "Variable x=5")
  ,("abs(-x)==x", Right 1)
  ,("1!=2", Right 1)
  ,("sin(m.pi)==0", Right 1)
  ,("/= = !=", Left . MsgMsg $ "Operator alias /= = !=")
  ,("1|0", Right 1)
  ,("1&0", Right 0)
  ,("0xff", Right 255)
  ,("0o45", Right 37)
  ,("0b1010", Right 10)
  ]

testsAH :: Tests
testsAH = map (Data.Bifunctor.second ((:+ 0) <$>)) [
     ("_", Right 0)
    ,("1", Right 1)
    ,("_", Right 1)
    ,("-1", Right (-1))
    ,("2+2", Right 4)
    ,("2-2-2-2", Right (-4))
    ,("1024/2/2/2", Right 128)
    ,("fun f(x) = x", Left . MsgMsg $ "Function f/1")
    ,("fun f(g,x) = g(x)", Left . MsgMsg $ "Function f/2")
    ,("fun p(x,y) = x - y", Left . MsgMsg $ "Function p/2")
    ,("fun h() = 2", Left . MsgMsg $ "Function h/0")
    ,("let hh = h()", Left . MsgMsg $ "Variable hh=2")
    ,("p(2,2)", Right 0)
    ,("op $(1,0) = x - y", Left . MsgMsg $ "Operator $ p=1 a=left")
    ,("2$2$2$2", Right (-4))
    ,("2$2-2$2", Right 0)
    ,("op $(5,0) = x - y", Left . MsgMsg $ "Operator $ p=5 a=left")
    ,("2$2-2$2", Right (-4))
    ,("2^3^4", Right 2417851639229258349412352)
    ,("2+2*2", Right 6)
    ,("-((1))", Right (-1))
    ,("-1^2", Right 1)
    ,("(2+2)*2", Right 8)
    ,("let x = 5",Left . MsgMsg $ "Variable x=5")
    ,("abs(-x) == x", Right 1)
    ,("1!=2", Right 1)
    ,("sin(m.pi)==0", Right 1)
    ,("1|0", Right 1)
    ,("1&0", Right 0)
    ]

defVar :: VarMap
defVar = [("m.pi", (:+0) $ toRational (pi :: Double)), ("m.e", (:+0) $ toRational . exp $ (1.0 :: Double)), ("_",0.0:+0.0)]

testLoop :: IO ()
testLoop = do
  g <- initStdGen
  TIO.putStrLn "Internal parser:"
  n1 <- loop tests (defVar, M.empty, opMap) g Internal 0
  -- TIO.putStrLn "\nMega parser:"
  -- n2 <- loop tests (defVar, M.empty, opMap) g Mega 0
  TIO.putStrLn "\nAlexHappy parser:"
  n3 <- loop testsAH (defVar, M.empty, opMap) g AH 0
  let n = n1 + {- n2 + -} n3
  if n == 0
    then exitSuccess
    else exitWith $ ExitFailure n
