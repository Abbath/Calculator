{-# LANGUAGE OverloadedStrings #-}

module Calculator.Opts.Dis where

import Calculator (Mode (..), defaultMaps, parseEval)
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg))
import Calculator.Types (showComplex)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text as TS (drop, isPrefixOf, pack, toLower)
import Data.Text.IO as TSIO (putStrLn)
import Data.Text.IO qualified as TIO
import System.Random (StdGen, getStdGen)

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
import System.Environment (lookupEnv)

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
    TIO.putStrLn "Started server"
    loadFile defaultConfig
    token <- fromMaybe "" <$> lookupEnv "DISCORD_TOKEN"
    g <- getStdGen
    userFacingError <-
        runDiscord $
            def
                { discordToken = TS.pack token
                , discordOnEvent = eventHandler g
                , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                } -- if you see OnLog error, post in the discord / open an issue
    TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: StdGen -> Event -> DiscordHandler ()
eventHandler g event = case event of
    MessageCreate m -> when (isCalc m && not (fromBot m)) $ do
        liftIO . TSIO.putStrLn $ "[" <> (TS.pack . show $ messageTimestamp m) <> "] " <> userName (messageAuthor m) <> ": " <> TS.drop 5 (messageContent m)
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        let res = parseEval Internal defaultMaps g (TS.drop 5 $ messageContent m)
        void $
            restCall
                ( R.CreateMessage
                    (messageChannelId m)
                    ( case res of
                        Left (MsgMsg msg, _) -> msg
                        Left (ErrMsg msg, _) -> msg
                        Right (r, _) -> showComplex r
                    )
                )
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCalc :: Message -> Bool
isCalc = ("calc" `TS.isPrefixOf`) . TS.toLower . messageContent