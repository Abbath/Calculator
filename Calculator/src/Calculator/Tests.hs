{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Calculator.Tests (testLoop) where

import Calculator.Builtins (defaultMaps)
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg), evalS)
import Calculator.Lexer (tloop)
import Calculator.Parser qualified as P
import Calculator.Types (EvalState (..), Maps (..), showT, unitlessValue, varmap)
import Control.Lens ((%~), (&))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
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
loop [] _ _ _ n = pure n
loop (x : xs) mps rgen bk n = do
  let sample = fst x
  if not $ T.null sample
    then do
      let e = case bk of
            Internal -> tloop sample >>= P.parse mps
      let t = case e of
            Left err -> (Left (ErrMsg err), EvalState mps rgen 16)
            Right r -> runState (runExceptT (evalS r)) (EvalState mps rgen 16)
      let tt = fst t
      do
        new_n <-
          if tt == snd x
            then do
              TIO.putStrLn $ "Passed: " <> sample
              pure 0
            else do
              TIO.putStrLn $ "Failed: " <> sample <> " expected: " <> showT x <> " received: " <> showT t
              pure 1
        let (m, g) = case t of
              (Right r, EvalState m_ g_ _) -> (m_ & varmap %~ M.insert "_" (unitlessValue r), g_)
              (Left _, EvalState m_ g_ _) -> (m_, g_)
        loop xs m g bk (n + new_n)
    else do
      TIO.putStrLn "Empty!"
      loop xs mps rgen bk n

pattern R :: (Eq a1, Num a1) => a1 -> Either a2 (Complex a1)
pattern R n = Right (n :+ 0)

tests :: Tests
tests =
  [ ("_", R 0)
  , ("1", R 1)
  , ("_", R 1)
  , ("-1", R (-1))
  , ("2+2", R 4)
  , ("2-2-2-2", R (-4))
  , ("1024/2/2/2", R 128)
  , ("f(x) = x", Left . MsgMsg $ "Function f/1")
  , ("f(g,x) = g(x)", Left . MsgMsg $ "Function f/2")
  , ("f(sin,m.pi)", R 0)
  , ("h() = 2", Left . MsgMsg $ "Function h/0")
  , ("hh = h()", Left . MsgMsg $ "Variable hh=2")
  , ("p(x,y) = x - y", Left . MsgMsg $ "Function p/2")
  , ("p(2,2)", R 0)
  , ("$(1,0) = x - y", Left . MsgMsg $ "Operator $ p=1 a=left")
  , ("2$2$2$2", R (-4))
  , ("2$2-2$2", R 0)
  , ("$(p(-),L) = x - y", Left . MsgMsg $ "Operator $ p=7 a=left")
  , ("2$2-2$2", R (-4))
  , ("2^3^4", R 2417851639229258349412352)
  , ("2+2*2", R 6)
  , ("-((1))", R (-1))
  , ("-1^2", R (-1))
  , ("(2+2)*2", R 8)
  , ("x = 5", Left . MsgMsg $ "Variable x=5")
  , ("abs(-x)==x", R 1)
  , ("1!=2", R 1)
  , ("sin(m.pi)==0", R 1)
  , ("/= = !=", Left . MsgMsg $ "Operator alias /= = !=")
  , ("1|0", R 1)
  , ("1&0", R 0)
  , ("0xff", R 255)
  , ("0o45", R 37)
  , ("0b1010", R 10)
  , ("int(f, 0, 1, 1e-9)", R 0.5)
  , ("(m.r > 0) & (m.r < 1)", R 1)
  , ("1_000_000", R 1000000)
  , ("x := 1 : x == 1", R 1)
  , ("b.false != b.true", R 1)
  , ("1 << 10", R 1024)
  , ("1024 >> 10", R 1)
  , ("pop(1023)", R 10)
  , ("fmt(\"%s %f %r\", \"test\", 2, 3)", R 36022907861344900240232095793)
  , ("sin `f` m.pi", R 0)
  , ("0 |> sin |> cos", R 1)
  , ("!6", R 720)
  , ("g(...) = ?v.0 + ?v.1 + v.n", Left . MsgMsg $ "Function g/0")
  , ("g(1)", R 2)
  , ("g(1,2)", R 5)
  , ("g(1,2,3)", R 6)
  , ("undef(x)", Left . MsgMsg $ "Removed: x")
  , ("x", Left . ErrMsg $ "No such variable : x")
  , ("sin 1 + sin 2", R 1.750768411633578202048522187542043842324818032246261334051724721522877705932023895585859281979981224571972325484933728542317200359259362921395331942523722661430914673812988282989904190981908507993127941308176455793670089899904951380449347198009490966796875)
  , ("gcd' (2+3) 5", R 5)
  , ("(-5) ^ 5", R (-3125))
  , ("sqrt(-1)", Right (0 :+ 1))
  , ("log(-1)", Right (0 :+ 3.1415926535897932384626433832795028841971693993751058209749445923078164062862066656221801262659750330277469299326429150650931940114238158780256065730465100356865497434884414384760991863982410321694316550044916649280825338141909242040128447115421295166015625))
  , ("6! + !6", R 1440)
  , ("!+(5, 2) = !x", Left . MsgMsg $ "Operator !+ p=5 a=none")
  , ("3!+ + !+3", R 12)
  , ("rad(180) == m.pi", R 1)
  , ("deg(m.pi) == 180", R 1)
  ]

testLoop :: IO ()
testLoop = do
  TIO.putStrLn "Internal parser:"
  n <- initStdGen >>= \g -> loop tests defaultMaps g Internal 0
  if n == 0
    then exitSuccess
    else exitWith $ ExitFailure n
