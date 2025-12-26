{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator.Opts.Ray where

import Calculator (Mode (..), parseEval)
import Calculator.Builtins (defaultEvalState)
import Calculator.Evaluator (MessageType (..))
import Calculator.Types (EvalState (..), showValue)
import Control.Lens (makeLenses, use, (%=), (.=))
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT, evalStateT)
import Data.Char (chr)
import Data.Text qualified as TS
import Raylib.Core qualified as RL
import Raylib.Core.Shapes qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Types qualified as RL
import Raylib.Util qualified as RL
import Raylib.Util.Colors qualified as RL
import System.Random (getStdGen)

data Zipper a = Zip [a] [a] deriving (Show)

zipLeft :: Zipper a -> Zipper a
zipLeft (Zip xs (y : ys)) = Zip (y : xs) ys
zipLeft z@(Zip _ []) = z

zipRight :: Zipper a -> Zipper a
zipRight (Zip (x : xs) ys) = Zip xs (x : ys)
zipRight z@(Zip [] _) = z

zipPut :: (Eq a) => a -> Zipper a -> Zipper a
zipPut x (Zip l r) = Zip l (x : r)

zipTop :: Zipper a -> a
zipTop (Zip l (x : xs)) = x
zipTop (Zip (x : xs) []) = x
zipTop (Zip [] []) = error "Empty zip"

zipEmpty :: Zipper a -> Bool
zipEmpty (Zip [] []) = True
zipEmpty (Zip _ _) = False

zipLeftEmpty :: Zipper a -> Bool
zipLeftEmpty (Zip [] _) = True
zipLeftEmpty (Zip _ _) = False

zipRightEmpty :: Zipper a -> Bool
zipRightEmpty (Zip _ []) = True
zipRightEmpty (Zip _ _) = False

data AppState = AS
  { _prompt :: TS.Text
  , _results :: [TS.Text]
  , _estate :: EvalState
  , _fc :: Integer
  , _phist :: Zipper TS.Text
  }
  deriving (Show)

makeLenses ''AppState

width :: Int
width = 800

height :: Int
height = 600

fps :: Int
fps = 60

fontSize :: Int
fontSize = 40

raylibLoop :: IO ()
raylibLoop = do
  g <- getStdGen
  let des = defaultEvalState{_gen = g}
  wr <- RL.initWindow width height "Calculator"
  RL.setTargetFPS fps
  RL.setTextLineSpacing 1
  RL.setWindowState [RL.WindowResizable]
  evalStateT raylibLoop' (AS "" [] des 0 (Zip [] []))

raylibLoop' :: StateT AppState IO ()
raylibLoop' = do
  wsc <- liftIO RL.windowShouldClose
  unless wsc $ do
    [w, h, c] <- liftIO $ sequence [RL.getRenderWidth, RL.getRenderHeight, RL.getCharPressed]
    prompt %= if c /= 0 then flip TS.snoc (chr c) else id
    [bp, up, down, ep] <- liftIO $ mapM RL.isKeyPressed [RL.KeyBackspace, RL.KeyUp, RL.KeyDown, RL.KeyEnter]
    prompt %= \pr -> if bp && not (TS.null pr) then TS.init pr else pr
    tw <- use prompt >>= liftIO . flip RL.measureText fontSize . TS.unpack
    checkHistory up down
    pr <- use prompt
    rt <- use results
    frame_counter <- use fc
    liftIO $ RL.drawing $ do
      RL.clearBackground RL.darkGray
      RL.drawRectangleRounded (RL.Rectangle 10 10 (fromIntegral w - 20) (fromIntegral fontSize)) 0.5 10 RL.gray
      RL.drawText (TS.unpack pr) 15 10 fontSize RL.lightGray
      when (frame_counter `mod` toInteger fps < toInteger fps `div` 2) $ RL.drawRectangle (17 + tw) 10 2 fontSize RL.lightGray
      forM_ (zip rt [0 ..]) (\(r, i) -> RL.drawText (TS.unpack r) 15 (fontSize + 12 + i * (fontSize + 2)) fontSize RL.lightGray)
    when ep $ do
      es <- use estate
      let y = parseEval Internal es pr
      let (res, es1) = case y of
            Right (r, es0) -> (showValue r, es0)
            Left (ErrMsg m, _) -> (m, es)
            Left (MsgMsg m, es0) -> (m, es0)
            Left (CmdMsg _, es0) -> ("", es0)
      prompt .= ""
      results .= take ((h - (fontSize + 12)) `div` (fontSize + 2)) (res : rt)
      estate .= es1
      phist %= zipPut pr
    fc %= (+ 1)
    raylibLoop'
 where
  checkHistory up down = do
    zt <- use phist
    if down && not (zipEmpty zt)
      then
        let ztl = zipLeft zt
         in do
              prompt .= zipTop zt
              unless (zipRightEmpty ztl) $ phist .= ztl
      else
        when (up && not (zipEmpty zt)) $
          if not $ zipLeftEmpty zt
            then
              let ztr = zipRight zt
               in do
                    prompt .= zipTop ztr
                    phist .= ztr
            else prompt .= ""
