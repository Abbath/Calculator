{-# LANGUAGE OverloadedStrings #-}

module Calculator.Opts.Dis where

import Calculator (Mode (..), parseEval)
import Calculator.Builtins (defaultEvalState)
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg))
import Calculator.Types (EvalState (..), showComplex)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text as TS (dropWhile, isPrefixOf, pack, toLower)
import Data.Text.IO as TSIO (putStrLn)
import Data.Text.IO qualified as TIO
import System.Random (getStdGen)
import UnliftIO.MVar

import Discord (
  DiscordHandler,
  RunDiscordOpts (discordOnEvent, discordOnLog, discordToken),
  def,
  restCall,
  runDiscord,
 )
import Discord.Requests qualified as R
import Discord.Types (Event (..), Message (messageTimestamp), User (userName), messageAuthor, messageChannelId, messageContent, messageId, userIsBot)

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Char (isSpace)
import System.Environment (lookupEnv)

discordCalculator :: IO ()
discordCalculator = do
  TIO.putStrLn "Started server"
  loadFile defaultConfig
  token <- fromMaybe "" <$> lookupEnv "DISCORD_TOKEN"
  mes <- getStdGen >>= \g -> newMVar defaultEvalState{_gen = g}
  userFacingError <-
    runDiscord $
      def
        { discordToken = TS.pack token
        , discordOnEvent = eventHandler mes
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue
  TIO.putStrLn userFacingError

eventHandler :: MVar EvalState -> Event -> DiscordHandler ()
eventHandler mes event = case event of
  MessageCreate m -> when (isCalc m && not (fromBot m)) $ do
    liftIO . TSIO.putStrLn $ "[" <> (TS.pack . show $ messageTimestamp m) <> "] " <> userName (messageAuthor m) <> ": " <> TS.dropWhile (not . isSpace) (messageContent m)
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    es <- takeMVar mes
    let res = parseEval Internal es (TS.dropWhile (not . isSpace) $ messageContent m)
    case res of
      Left (MsgMsg msg, nes) -> do
        putMVar mes nes
        respond m msg
      Left (ErrMsg msg, _) -> respond m msg
      Right (r, nes) -> do
        putMVar mes nes
        respond m $ showComplex r
  _ -> pure ()
 where
  respond m rsp =
    void $
      restCall
        ( R.CreateMessage
            (messageChannelId m)
            rsp
        )

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCalc :: Message -> Bool
isCalc = (\m -> any (`TS.isPrefixOf` m) ["calc", "!c"]) . TS.toLower . messageContent
