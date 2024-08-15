{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Tests (testLoop) where

import Calculator.Builtins (opMap)
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg), evalS)
import Calculator.Lexer (tloop)
import Calculator.Parser qualified as P
import Calculator.Types (EvalState (..), Maps (..), VarMap, showT, varmap)
import Control.Lens ((%~), (&))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Complex (Complex (..))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO as TIO (putStrLn)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.Random (StdGen, initStdGen)

data Backend = Internal deriving (Show)

type Tests = [(Text, Either MessageType (Complex Rational))]

loop :: Tests -> Maps -> StdGen -> Backend -> Int -> IO Int
loop [] _ _ _ n = return n
loop (x : xs) mps rgen bk n = do
  let sample = fst x
  if not $ T.null sample
    then do
      let e = case bk of
            Internal -> tloop sample >>= P.parse mps
      let t = case e of
            Left err -> (Left (ErrMsg err), EvalState mps rgen)
            Right r -> runState (runExceptT (evalS r)) (EvalState mps rgen)
      let tt = fst t
      do
        new_n <-
          if tt == snd x
            then do
              TIO.putStrLn $ "Passed: " <> sample
              return 0
            else do
              TIO.putStrLn $ "Failed: " <> sample <> " expected: " <> showT x <> " received: " <> showT t
              return 1
        let (m, g) = case t of
              (Right r, EvalState m_ g_) -> (m_ & varmap %~ M.insert "_" r, g_)
              (Left _, EvalState m_ g_) -> (m_, g_)
        loop xs m g bk (n + new_n)
    else do
      TIO.putStrLn "Empty!"
      loop xs mps rgen bk n

tests :: Tests
tests =
  map
    (Data.Bifunctor.second ((:+ 0) <$>))
    [ ("_", Right 0)
    , ("1", Right 1)
    , ("_", Right 1)
    , ("-1", Right (-1))
    , ("2+2", Right 4)
    , ("2-2-2-2", Right (-4))
    , ("1024/2/2/2", Right 128)
    , ("f(x) = x", Left . MsgMsg $ "Function f/1")
    , ("f(g,x) = g(x)", Left . MsgMsg $ "Function f/2")
    , ("f(sin,m.pi)", Right 0)
    , ("h() = 2", Left . MsgMsg $ "Function h/0")
    , ("hh = h()", Left . MsgMsg $ "Variable hh=2")
    , ("p(x,y) = x - y", Left . MsgMsg $ "Function p/2")
    , ("p(2,2)", Right 0)
    , ("$(1,0) = x - y", Left . MsgMsg $ "Operator $ p=1 a=left")
    , ("2$2$2$2", Right (-4))
    , ("2$2-2$2", Right 0)
    , ("$(5,0) = x - y", Left . MsgMsg $ "Operator $ p=5 a=left")
    , ("2$2-2$2", Right (-4))
    , ("2^3^4", Right 2417851639229258349412352)
    , ("2+2*2", Right 6)
    , ("-((1))", Right (-1))
    , ("-1^2", Right 1)
    , ("(2+2)*2", Right 8)
    , ("x = 5", Left . MsgMsg $ "Variable x=5")
    , ("abs(-x)==x", Right 1)
    , ("1!=2", Right 1)
    , ("sin(m.pi)==0", Right 1)
    , ("/= = !=", Left . MsgMsg $ "Operator alias /= = !=")
    , ("1|0", Right 1)
    , ("1&0", Right 0)
    , ("0xff", Right 255)
    , ("0o45", Right 37)
    , ("0b1010", Right 10)
    , ("int(f, 0, 1, 1e-9)", Right 0.5)
    , ("(m.r > 0) & (m.r < 1)", Right 1)
    , ("1_000_000", Right 1000000)
    , ("x := 1 : x == 1", Right 1)
    , ("b.false != b.true", Right 1)
    , ("1 << 10", Right 1024)
    , ("1024 >> 10", Right 1)
    , ("pop(1023)", Right 10)
    , ("fmt(\"%s %f %r\", \"test\", 2, 3)", Left . MsgMsg $ "test 2 3 / 1")
    , ("sin `f` m.pi", Right 0)
    , ("0 |> sin |> cos", Right 1)
    , ("!6", Right 720)
    , ("g(...) = ?v.0 + ?v.1 + v.n", Left . MsgMsg $ "Function g/0")
    , ("g(1)", Right 2)
    , ("g(1,2)", Right 5)
    , ("g(1,2,3)", Right 6)
    , ("undef(x)", Left . MsgMsg $ "Removed: x")
    , ("x", Left . ErrMsg $ "No such variable : x")
    , ("sin 1 + sin 2", Right 1.750768411633578214292583652422763407230377197265625)
    , ("gcd' (2+3) 5", Right 5)
    ]

defVar :: VarMap
defVar =
  [ ("m.pi", (:+ 0) $ toRational (pi :: Double))
  , ("m.e", (:+ 0) $ toRational . exp $ (1.0 :: Double))
  , ("_", 0.0 :+ 0.0)
  , ("m.phi", toRational ((1 + sqrt 5) / 2 :: Double) :+ 0)
  , ("m.r", 0.0 :+ 0)
  , ("b.true", 1.0 :+ 0)
  , ("b.false", 0.0 :+ 0)
  ]

testLoop :: IO ()
testLoop = do
  g <- initStdGen
  TIO.putStrLn "Internal parser:"
  n <- loop tests (Maps defVar M.empty opMap M.empty) g Internal 0
  if n == 0
    then exitSuccess
    else exitWith $ ExitFailure n
