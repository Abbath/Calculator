{-# LANGUAGE OverloadedStrings #-}

module Calculator.Opts.Tel where

import Calculator (Mode (..), parseEval)
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
bot :: Mode -> StdGen -> BotApp Model Action
bot mode gen =
  BotApp
    { botInitialModel = Model defaultEvalState{_gen = gen}
    , botAction = flip handleUpdate
    , botHandler = handleAction mode
    , botJobs = []
    }

{- | How to process incoming 'Telegram.Update's
and turn them into 'Action's.
-}
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate (Reply <$> Telegram.Bot.Simple.UpdateParser.text)

-- | How to handle 'Action's.
handleAction :: Mode -> Action -> Model -> Eff Action Model
handleAction _ NoAction model = pure model
handleAction mode (Reply msg) model =
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
        (let (EvalState mps rgen pr) = getMaps model in parseEval mode (EvalState mps rgen pr) msg)

-- | Run bot with a given 'Telegram.Token'.
run :: Mode -> Telegram.Token -> IO ()
run mode token = do
  g <- initStdGen
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId (bot mode g)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: Mode -> IO ()
telegramSimple mode = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run mode
