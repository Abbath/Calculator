{-# LANGUAGE OverloadedStrings, TupleSections, OverloadedLists #-}
module Calculator (Mode(..), evalLoop, webLoop, telegramSimple, evalFile) where

import Calculator.AlexLexer (alexScanTokens)
import Calculator.Builtins (opMap, names, funMap, defVar, getPrecedences, getFakePrecedences)
import Calculator.Css (getCss, postCss)
import Calculator.Evaluator (evalS, MessageType(..))
import qualified Calculator.HappyParser as HP
import Calculator.HomebrewLexer (tloop)
import qualified Calculator.MegaParser as CMP
import Calculator.Parser (parse)
import Calculator.Types
    ( preprocess,
      showT,
      Expr,
      ListTuple,
      Maps,
      opsToList,
      opsFromList,
      getPrA,
      funsToList,
      funsFromList,
      showComplex,
      EvalState (EvalState),
      maps)
import Clay (render)
import Control.Arrow (first, left, second)
import Control.Lens ((%~), (&), (^.), _1, _2, _3)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    ReaderT (runReaderT),
    when,
  )
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf, union)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as TS
import qualified Data.Text.IO as TSIO
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai (Request (remoteHost))
import Network.Wai.Middleware.Static
  ( addBase,
    noDots,
    staticPolicy,
    (>->),
  )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
  ( action,
    autofocus,
    enctype,
    method,
    name,
    type_,
    value,
  )
import Web.Scotty
  ( file,
    get,
    html,
    middleware,
    param,
    post,
    redirect,
    request,
    scotty,
  )

import System.Console.Haskeline
    ( getInputLine,
      completeWord,
      runInputT,
      Completion(..),
      CompletionFunc,
      InputT,
      Settings(..),
      withInterrupt,
      handleInterrupt, outputStrLn)
import System.Directory ( findFile, getHomeDirectory, removeFile )
import System.Random ( randomIO, initStdGen, StdGen )
import qualified Text.Megaparsec               as MP
import           Text.Read                     (readMaybe)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import qualified Telegram.Bot.API                 as Telegram
import Telegram.Bot.Simple
    ( getEnvToken,
      startBot_,
      conversationBot,
      (<#),
      replyText,
      BotApp(..),
      Eff )
import Telegram.Bot.Simple.UpdateParser ( parseUpdate, text )
import qualified Control.Exception as CE
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Complex

newtype Model = Model {getMaps :: EvalState}

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply !TS.Text  -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: Mode -> StdGen -> BotApp Model Action
bot mode gen = BotApp
  { botInitialModel = Model (EvalState (defVar, M.empty, opMap) gen M.empty)
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
          (let (EvalState mps rgen _) = getMaps model in parseEval mode mps rgen msg)

-- | Run bot with a given 'Telegram.Token'.
run :: Mode -> Telegram.Token -> IO ()
run mode token = do
  g <- initStdGen
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId (bot mode g)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: Mode -> IO ()
telegramSimple mode = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run mode

data Mode = Internal | Megaparsec | AlexHappy deriving Show

parseString :: Mode -> TS.Text -> Maps -> Either TS.Text Expr
parseString m s ms = case m of
                       Megaparsec ->
                         left showT (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (s <> "\n"))
                       Internal ->
                         tloop s >>= parse (getPrecedences (ms^._3) <> getFakePrecedences (ms^._2))
                       AlexHappy -> Right $ preprocess . HP.parse . Calculator.AlexLexer.alexScanTokens $ TS.unpack s

evalExprS :: Either TS.Text Expr -> Maps -> StdGen -> Either (MessageType, EvalState) (Complex Rational, EvalState)
evalExprS t mps g = either (Left . (, EvalState mps g M.empty) . ErrMsg) ((\(r, s) -> either (Left . (,s)) (Right . (,s)) r) . getShit) t
  where getShit e = let a = runExceptT (evalS e) in S.runState a (EvalState mps g M.empty)

type StateData = [String]

completionGenerator :: String -> StateT StateData IO [Completion]
completionGenerator s = do
  sd <- S.get
  return $ map (\x -> Completion {replacement = x, display = x, isFinished = False})
         $ filter (isPrefixOf s) (names `union` sd)

replCompletion :: CompletionFunc (StateT StateData IO)
replCompletion = completeWord Nothing " " completionGenerator

replSettings :: Settings (StateT StateData IO)
replSettings = Settings
  { complete       = replCompletion
  , historyFile    = Nothing
  , autoAddHistory = True
  }

extractNames :: Maps -> [String]
extractNames (v, f, o) = map TS.unpack $ M.keys o <> M.keys v <> map fst (M.keys f)

loop :: Mode -> Maps -> IO ()
loop mode mps = do
    g <- initStdGen
    hd <- getHomeDirectory
    x <- CE.try $ flip S.evalStateT [] $ runInputT
       (replSettings { historyFile = Just (hd ++ "/.mycalchist")})
       (loop' mode mps g)
    case x of
      Left (CE.ErrorCall s) -> do
        putStrLn s
        Calculator.loop mode mps
      Right _ -> return ()
  where
  removeLocals = _1 %~ M.filterWithKey (\k v -> not $ "_." `TS.isInfixOf` k)
  loop' :: Mode -> Maps -> StdGen -> InputT (StateT StateData IO) ()
  loop' md ms g = do
    S.lift $ S.modify (\s -> s `union` extractNames ms)
    input <- getInputLine "> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just x -> handleInterrupt (outputStrLn "Cancelled." >> loop' md ms g) . withInterrupt $ do
          let y = parseEval md ms g (TS.pack x)
          case y of
            Left (err, EvalState m ng _) -> do
              let m' = removeLocals m
              case err of
                MsgMsg msg -> liftIO $ TSIO.putStrLn msg
                ErrMsg emsg -> liftIO $ TSIO.putStrLn ("Error: " <> emsg)
              loop' md m' ng
            Right (r, EvalState m ng _) -> do
              let m' = removeLocals m
              liftIO . TSIO.putStrLn $ showComplex r
              loop' md (m' & _1 %~ M.insert "_" r) ng

interpret :: FilePath -> Mode -> Maps -> IO ()
interpret path mode mps = do
    g <- initStdGen
    source <- TS.lines <$> TSIO.readFile path
    x <- CE.try $ flip S.evalStateT [] $ loop' source mode mps g
    case x of
      Left (CE.ErrorCall s) -> do
        putStrLn s
        exitWith (ExitFailure 1)
      Right _ -> exitSuccess
  where
  loop' :: [TS.Text] -> Mode -> Maps -> StdGen -> StateT StateData IO ()
  loop' [] _ _ _ = return ()
  loop' src@(l:ls) md ms g = do
    S.modify (\s -> s `union` extractNames ms)
    case l of
      "quit" -> return ()
      x -> do
        let y = parseEval md ms g x
        case y of
          Left (err, EvalState m ng _) -> do
            case err of
              MsgMsg msg -> liftIO $ TSIO.putStrLn msg
              ErrMsg emsg -> liftIO $ TSIO.putStrLn ("Error: " <> emsg)
            loop' ls md m ng
          Right (r, EvalState m ng _) -> do
            liftIO . TSIO.putStrLn $ showComplex r
            loop' ls md (m & _1 %~ M.insert "_" r) ng

parseEval :: Mode -> Maps -> StdGen -> TS.Text -> Either (MessageType, EvalState) (Complex Rational, EvalState)
parseEval md ms g x = evalExprS (parseString md x ms) ms g

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m (defVar, funMap, opMap)

evalFile :: FilePath -> IO ()
evalFile f =  Calculator.interpret f Internal (defVar, funMap, opMap)

updateIDS :: Integer -> IO ()
updateIDS i = do
    f <- BS.readFile "ids"
    let idsm = readMaybe (BS.unpack f) :: Maybe [(Integer, Integer)]
    tm <- round `fmap` getPOSIXTime
    let ids = fromMaybe [] idsm
    mapM_ (\(_, ff) -> do
             let logname = "log" ++ show ff ++ ".dat";
             let storagename = "storage" ++ show ff ++ ".dat"
             b1 <- findFile ["."] storagename
             b2 <- findFile ["."] logname
             when (isJust b1) $ removeFile storagename
             when (isJust b2) $ removeFile logname) $ filter (\(a,_) -> tm-a > 60*60) ids
    BS.writeFile "ids" $ BS.pack $ show $ if i `elem` map snd ids
       then map (\(a,b) -> if b == i then (tm,i) else (a,b)) ids
       else (tm,i) : filter (\(a,_) -> tm - a < 60*60) ids

webLoop :: Int -> Mode -> IO ()
webLoop port mode = do
  g <- initStdGen
  ref <- newIORef g
  Web.Scotty.scotty port $ do
    Web.Scotty.middleware $ Network.Wai.Middleware.Static.staticPolicy (Network.Wai.Middleware.Static.noDots Network.Wai.Middleware.Static.>-> Network.Wai.Middleware.Static.addBase "Static/images") -- for future
    Web.Scotty.get "/" $ do
      req <- Web.Scotty.request
      let sa = remoteHost req
      liftIO $ print sa
      do
        let x = liftIO (randomIO :: IO Integer)
        y <- x
        f <- liftIO $ findFile ["."] ("log" ++ show y ++ ".dat")
        if isJust f
          then Web.Scotty.redirect "/"
          else do
            liftIO $ updateIDS (abs y)
            Web.Scotty.redirect $ T.append "/" $ T.pack (show $ abs y)
    Web.Scotty.get "/favicon.ico" $ Web.Scotty.file "./Static/favicon.ico"
    Web.Scotty.get "/:id" $ do
      iD <- Web.Scotty.param "id"
      liftIO $ BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") (B.toStrict . encode $ ((map (\(k, v) -> (k, (realPart v, imagPart v))) . M.toList $ defVar, [], opsToList opMap) :: ListTuple ))
      liftIO $ BS.writeFile ("log" ++ T.unpack iD ++ ".dat") "[]"
      Web.Scotty.html $ renderHtml
        $ H.html $ H.body $ do
          H.h1 $ H.toHtml ("Calculator" :: TS.Text)
          H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
            H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
          H.style $ H.toHtml . render $ getCss
    Web.Scotty.post "/clear/:id" $ do
      iD <- Web.Scotty.param "id"
      Web.Scotty.redirect $ T.append "/" iD
    Web.Scotty.post "/:id" $ do
      iD <- Web.Scotty.param "id"
      liftIO $ updateIDS (read . T.unpack $ iD :: Integer)
      fs <- Web.Scotty.param "foo"
      f1 <- liftIO $ findFile ["."] ("storage" ++ T.unpack iD ++ ".dat")
      f2 <- liftIO $ findFile ["."] ("log" ++ T.unpack iD ++ ".dat")
      if isNothing f1 || isNothing f2
        then Web.Scotty.redirect "/"
        else do
          let logname = "log" ++ T.unpack iD ++ ".dat"
          let storagename = "storage" ++ T.unpack iD ++ ".dat"
          rest <- liftIO $ BS.readFile logname
          env <- liftIO $ BS.readFile storagename
          let ms = maybe
                    (error "Cannot decode storage")
                    listsToMaps
                    (decode (B.fromStrict env) :: Maybe ListTuple)
          let lg = fromMaybe (error "Cannot decode log") (decode (B.fromStrict rest) :: Maybe [(TS.Text, TS.Text)])
          let t = parseString mode fs ms
          gen <- liftIO $ readIORef ref
          let res = evalExprS t ms gen
          let txt = let (ress, EvalState mps ng _) = either
                          Prelude.id
                          (\(r, mg) -> (MsgMsg . showComplex $ r, (maps . _1 %~ M.insert "_" r) mg))
                          res
                    in do
                        storeMaps storagename mps
                        liftIO $ writeIORef ref ng
                        return $ (fs, unpackMsg ress) : lg
          rtxt <- liftIO txt
          liftIO $ BS.writeFile logname . B.toStrict . encode $ rtxt
          Web.Scotty.html $ renderHtml
              $ H.html $
                  H.body $ do
                    H.h1 $ H.toHtml ("Calculator" :: TS.Text)
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
                      H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/clear/" iD) $
                      H.input H.! type_ "submit" H.! value "Clear history"
                    H.table $ mapM_ (\(x,y) -> H.tr $ (H.td . H.toHtml $ x) >> (H.td . H.toHtml $ y)) rtxt
                    H.style $ H.toHtml . render $ postCss
  where
    storeMaps s = BS.writeFile s . B.toStrict . encode . mapsToLists
    mapsToLists (a, b, c) = (map (\(k, v) -> (k, (realPart v, imagPart v))) . M.toList $ a, funsToList b, opsToList c)
    listsToMaps (a, b, c) = (M.fromList . map (\(k, (vr, vi)) -> (k, vr:+vi)) $ a, M.union funMap $ funsFromList b, M.union opMap $ opsFromList c)
    unpackMsg (MsgMsg m) = m
    unpackMsg (ErrMsg e) = e
