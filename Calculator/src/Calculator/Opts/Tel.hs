{-# LANGUAGE OverloadedStrings #-}

module Calculator.Opts.Tel where

import Calculator (parseEval)
import Calculator.Builtins (defaultEvalState)
import Calculator.Evaluator (MessageType (..))
import Calculator.Types (EvalState (..), showValue)
import Control.Arrow (first, second)
import Data.Text qualified as TS
import System.Random
import Telegram.Bot.API qualified as Telegram
import Telegram.Bot.Simple (
  BotApp (..),
  Eff,
  conversationBot,
  getEnvToken,
  replyText,
  startBot_,
  (<#),
 )
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text)

newtype Model = Model {getMaps :: EvalState}

-- | Actions bot can perform.
data Action
  = -- | Perform no action.
    NoAction
  | -- | Reply some text.
    Reply !TS.Text
  deriving (Show)

-- | Bot application.
bot :: StdGen -> BotApp Model Action
bot gen =
  BotApp
    { botInitialModel = Model defaultEvalState{_gen = gen}
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs = []
    }

{- | How to process incoming 'Telegram.Update's
and turn them into 'Action's.
-}
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate (Reply <$> Telegram.Bot.Simple.UpdateParser.text)

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Reply msg) model =
  model2 <# do
    replyText $ case response of
      MsgMsg mmsg -> mmsg
      ErrMsg emsg -> "Error: " <> emsg
      CmdMsg _ -> ""
    pure NoAction
 where
  (response, model2) =
    second Model $
      either
        Prelude.id
        (first (MsgMsg . showValue))
        (let (EvalState mps rgen pr) = getMaps model in parseEval (EvalState mps rgen pr) msg)

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  g <- initStdGen
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId (bot g)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: IO ()
telegramSimple = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
