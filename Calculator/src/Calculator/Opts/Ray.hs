{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator.Opts.Ray where

import Calculator (Mode (..), defaultMaps, parseEval)
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg))
import Calculator.Types (EvalState (..), showComplex)
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

raylibLoop :: IO ()
raylibLoop = do
  g <- getStdGen
  let des = EvalState defaultMaps g
  evalStateT raylibLoop' (AS "" [] des 0 (Zip [] []))

raylibLoop' :: StateT AppState IO ()
raylibLoop' = do
  let width = 800
  let height = 600
  let fps = 60
  RL.withWindow width height "Calculator" fps $ \wr -> do
    RL.whileWindowOpen0 $ do
      w <- liftIO RL.getRenderWidth
      h <- liftIO RL.getRenderHeight
      c <- liftIO RL.getCharPressed
      prompt %= if c /= 0 then flip TS.snoc (chr c) else id
      bp <- liftIO $ RL.isKeyPressed RL.KeyBackspace
      prompt %= \pr -> if bp && not (TS.null pr) then TS.init pr else pr
      tw <- use prompt >>= liftIO . flip RL.measureText 20 . TS.unpack
      up <- liftIO $ RL.isKeyPressed RL.KeyUp
      down <- liftIO $ RL.isKeyPressed RL.KeyDown
      checkHistory up down
      pr <- use prompt
      rt <- use results
      frame_counter <- use fc
      liftIO $ RL.drawing $ do
        RL.clearBackground RL.darkGray
        RL.drawRectangleRounded (RL.Rectangle 10 10 (fromIntegral w - 20) 20) 0.5 10 RL.gray
        RL.drawText (TS.unpack pr) 15 10 20 RL.lightGray
        when (frame_counter `mod` toInteger fps < toInteger fps `div` 2) $ RL.drawRectangle (17 + tw) 10 2 20 RL.lightGray
        forM_ (zip rt [0 ..]) (\(r, i) -> RL.drawText (TS.unpack r) 15 (32 + i * 22) 20 RL.lightGray)
      ep <- liftIO $ RL.isKeyPressed RL.KeyEnter
      when ep $ do
        es <- use estate
        let y = parseEval Internal es pr
        let (res, es1) = case y of
              Right (r, es0) -> (showComplex r, es0)
              Left (ErrMsg m, _) -> (m, es)
              Left (MsgMsg m, es0) -> (m, es0)
        prompt .= ""
        results .= take ((h - 32) `div` 22) (res : rt)
        estate .= es1
        phist %= zipPut pr
      fc %= (+ 1)
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