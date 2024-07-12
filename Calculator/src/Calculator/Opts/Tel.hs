{-# LANGUAGE OverloadedStrings #-}
module Calculator.Opts.Tel where
import Control.Arrow (first, second)
import qualified Telegram.Bot.API  as Telegram
import Telegram.Bot.Simple
    ( getEnvToken,
      startBot_,
      conversationBot,
      (<#),
      replyText,
      BotApp(..),
      Eff )
import Telegram.Bot.Simple.UpdateParser ( parseUpdate, text )
import Calculator (Mode (..), defaultMaps, parseEval)
import Calculator.Types (EvalState (..), showComplex)
import Data.Text qualified as TS
import System.Random
import Calculator.Evaluator (MessageType (ErrMsg, MsgMsg))

newtype Model = Model {getMaps :: EvalState}

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply !TS.Text  -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: Mode -> StdGen -> BotApp Model Action
bot mode gen = BotApp
  { botInitialModel = Model (EvalState defaultMaps gen)
  , botAction = flip handleUpdate
  , botHandler = handleAction mode
  , botJobs = []
  }

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate (Reply <$> Telegram.Bot.Simple.UpdateParser.text)

-- | How to handle 'Action's.
handleAction :: Mode -> Action -> Model -> Eff Action Model
handleAction _ NoAction model = pure model
handleAction mode (Reply msg) model = model2 <# do
  replyText $ case response of
    MsgMsg mmsg -> mmsg
    ErrMsg emsg -> "Error: " <> emsg
  pure NoAction
  where (response, model2) = second Model $ either
          Prelude.id
          (first (MsgMsg . showComplex))
          (let (EvalState mps rgen) = getMaps model in parseEval mode mps rgen msg)

-- | Run bot with a given 'Telegram.Token'.
run :: Mode -> Telegram.Token -> IO ()
run mode token = do
  g <- initStdGen
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId (bot mode g)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: Mode -> IO ()
telegramSimple mode = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run mode
