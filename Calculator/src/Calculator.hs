{-# LANGUAGE OverloadedStrings, OverloadedLists, CPP #-}
module Calculator (
  Mode(..),
  CompileMode(..),
  evalLoop,
  webLoop,
#ifdef TELEGRAM
  telegramSimple,
#endif
#ifdef RAYLIB
  raylibLoop,
#endif
  evalFile,
  compileAndRunFile) where

import Calculator.Builtins (defVar, funMap, names, opMap)
import Calculator.Css (getCss, postCss)
import Calculator.Evaluator (evalS, MessageType(..))
import Calculator.Lexer (tloop)
import qualified Calculator.Parser as P
import qualified Calculator.Compiler as C
import Calculator.Types
    ( Expr(Seq, Imprt),
      ListTuple,
      Maps(..),
      opsToList,
      opsFromList,
      funsToList,
      funsFromList,
      showComplex,
      EvalState (EvalState),
      maps,
      extractFormat,
      zipFormat,
      isFormat, varmap, opmap, funmap)
import Clay (render)
import Control.Lens ((%~), (&), (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
  ( MonadIO (liftIO),
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
    post,
    redirect,
    request,
    scotty,
    captureParam,
    formParam,
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
import System.FilePath (replaceExtension)
import System.Random ( randomIO, initStdGen, StdGen, getStdGen )
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import qualified Control.Exception as CE
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Complex ( imagPart, realPart, Complex(..) )
import Text.Read ( readMaybe )

#ifdef RAYLIB
import qualified Raylib.Core as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import qualified Raylib.Core.Shapes as RL
import qualified Raylib.Types as RL
import qualified Raylib.Core.Text as RL
import Data.Char (chr)
import Control.Monad (forM_)
import Calculator.Types (gen)
import Debug.Trace

data Zipper a = Zip [a] [a] deriving Show

zipLeft :: Zipper a -> Zipper a
zipLeft (Zip xs (y:ys)) = Zip (y:xs) ys
zipLeft z@(Zip _ []) = z

zipRight :: Zipper a -> Zipper a
zipRight (Zip (x:xs) ys) = Zip xs (x:ys)
zipRight z@(Zip [] _) = z

zipPut :: Eq a => a -> Zipper a -> Zipper a
zipPut x (Zip l r) = Zip l (x:r)

zipTop :: Zipper a -> a
zipTop (Zip l (x:xs)) = x
zipTop (Zip (x:xs) []) = x
zipTop (Zip [] []) = error "Empty zip"

zipEmpty :: Zipper a -> Bool
zipEmpty (Zip [] []) = True
zipEmpty (Zip _ _) = False

zipLeftEmpty :: Zipper a -> Bool
zipLeftEmpty (Zip [] _) = True
zipLeftEmpty (Zip _ _) = False

data AppState = AS TS.Text [TS.Text] EvalState Integer (Zipper TS.Text)

raylibLoop :: IO AppState
raylibLoop = do
  let width = 800
  let height = 600
  let fps = 60
  g <- getStdGen
  let des = EvalState defaultMaps g
  RL.withWindow width height "Calculator" fps (\wr -> do
    RL.whileWindowOpen (\(AS t rt es fc zt) -> do
      w <- RL.getRenderWidth
      h <- RL.getRenderHeight
      c <- RL.getCharPressed
      let t0 = if c /= 0 then TS.snoc t (chr c) else t
      bp <- RL.isKeyPressed RL.KeyBackspace
      let t1 = if bp && not (TS.null t0) then TS.init t0 else t0
      tw <- RL.measureText (TS.unpack t1) 20
      up <- RL.isKeyPressed RL.KeyUp
      down <- RL.isKeyPressed RL.KeyDown
      let (t2, zt1) = if down && not (zipEmpty zt)
          then (zipTop zt, zipLeft zt)
          else if up && not (zipEmpty zt)
            then if not $ zipLeftEmpty zt
              then let ztr = zipRight zt in (zipTop ztr, ztr)
              else ("", zt)
            else (t1, zt)
      trace (show zt1) return ()
      RL.drawing $ do
        RL.clearBackground RL.darkGray
        RL.drawRectangleRounded (RL.Rectangle 10 10 (fromIntegral w - 20) 20) 0.5 10 RL.gray
        RL.drawText (TS.unpack t2) 10 10 20 RL.lightGray
        when (fc `mod` toInteger fps < toInteger fps `div` 2) $ RL.drawRectangle (12 + tw) 10 2 20 RL.lightGray
        forM_ (zip rt [0..]) (\(r, i) -> RL.drawText (TS.unpack r) 10 (32 + i * 22) 20 RL.lightGray)
      ep <- RL.isKeyPressed RL.KeyEnter
      if ep
        then do
          let y = parseEval Internal (es ^. maps) (es ^. gen) t2
          let (rt1, es1) = case y of
                      Right (r, es0) -> (showComplex r : rt, es0)
                      Left (ErrMsg m, _) -> (m : rt, es)
                      Left (MsgMsg m, es0) -> (m : rt, es0)
          return $ AS "" (take ((h - 32) `div` 22) rt1) es1 (fc + 1) (zipPut t2 zt1)
        else return $ AS t2 rt es (fc + 1) zt1) (AS "" [] des 0 (Zip [] [])))
#endif


#ifdef TELEGRAM
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

#endif

data Mode = Internal deriving Show

parseString :: Mode -> TS.Text -> Maps -> Either TS.Text Expr
parseString m s ms = case m of
                       Internal -> tloop s >>= P.parse ms

evalExprS :: Either TS.Text Expr -> Maps -> StdGen -> Either (MessageType, EvalState) (Complex Rational, EvalState)
evalExprS t mps g = either (Left . (, EvalState mps g) . ErrMsg) ((\(r, s) -> either (Left . (,s)) (Right . (,s)) r) . getShit) t
  where getShit e = let a = runExceptT (evalS e) in S.runState a (EvalState mps g)

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
extractNames ms = map TS.unpack $ M.keys (ms^.opmap) <> M.keys (ms^.varmap) <> map fst (M.keys (ms^.funmap))

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
  removeLocals = varmap %~ M.filterWithKey (\k v -> not $ "_." `TS.isInfixOf` k)
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
            Left (err, EvalState m ng) -> do
              let m' = removeLocals m
              case err of
                MsgMsg msg -> liftIO $ TSIO.putStrLn msg
                ErrMsg emsg -> liftIO $ TSIO.putStrLn ("Error: " <> emsg)
              loop' md m' ng
            Right (r, EvalState m ng) -> do
              let m' = removeLocals m
              liftIO . TSIO.putStrLn . showComplex $ r
              loop' md (m' & varmap %~ M.insert "_" r) ng

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
          Left (err, EvalState m ng) -> do
            case err of
              MsgMsg msg -> liftIO $ TSIO.putStrLn msg
              ErrMsg emsg -> liftIO $ TSIO.putStrLn ("Error: " <> emsg)
            loop' ls md m ng
          Right (r, EvalState m ng) -> do
            liftIO . TSIO.putStrLn $ showComplex r
            loop' ls md (m & varmap %~ M.insert "_" r) ng

data CompileMode = CompStore | CompLoad | CompRead deriving (Show)

parseNumber :: TS.Text -> Complex Rational
parseNumber t = case TS.split (=='j') t of
  [r, i] -> (toRational . read @Double . TS.unpack $ r) :+ (toRational . read @Double . TS.unpack $ i)
  [r] -> (:+0) . toRational . read @Double . TS.unpack $ r
  _ -> 0 :+ 0

compileAndRun :: FilePath -> CompileMode -> Maps -> IO ()
compileAndRun path mode mps = case mode of
  CompRead -> do
    parsed <- parseSource mps path
    g <- initStdGen
    compileAst parsed $ \bc -> execute mps $ C.emptyVM bc g
  CompLoad -> do
    bc <- fromMaybe C.emptyChunk <$> C.loadBc path
    g <- initStdGen
    execute mps $ C.emptyVM bc g
  CompStore -> do
    parsed <- parseSource mps path
    compileAst parsed $ C.storeBc (replaceExtension path ".bin")
  where
    loop' :: [TS.Text] -> Maps -> StateT [Expr] IO ()
    loop' [] _  = S.modify reverse
    loop' (l:ls) ms = case parseString Internal l ms of
      Left err -> do
        liftIO $ TSIO.putStrLn err
      Right res -> case res of
        (Imprt filepath) -> do
          source <- liftIO $ TS.lines <$> TSIO.readFile (T.unpack . T.fromStrict $ filepath)
          loop' (source ++ ls) mps
        _ -> do
          S.modify (res:)
          loop' ls mps
    compileAst ast act = case C.compile mps (Seq ast) of
      Left err -> TSIO.putStrLn err
      Right bc -> act bc
    parseSource ms fp = do
      source <- TS.lines <$> TSIO.readFile fp
      flip S.execStateT [] $ loop' source ms
    execute ms vm = case C.interpretBcVM ms vm of
      (Left err, _) -> TSIO.putStrLn err
      (Right status, new_vm) -> case status of
        C.IrOk -> putStrLn "Success"
        C.IrIO operation fmt -> case operation of
          C.OpInput -> do
            line <- TSIO.getLine
            execute ms $ C.injectValue (parseNumber line) new_vm
          C.OpOutput -> case C.ejectValue new_vm of
            (Nothing, _) -> execute ms new_vm
            (Just v, newer_vm) -> do
              TSIO.putStrLn (showComplex v)
              execute ms newer_vm
          C.OpFmt -> case fmt of
            Nothing -> putStrLn "Nothing to format."
            Just fmts -> case extractFormat fmts of
              Left err -> TSIO.putStrLn err
              Right fmtcs -> let (values, newer_vm) = C.ejectValues (length . filter isFormat $ fmtcs) new_vm
                             in case zipFormat fmtcs values of
                                Left err -> TSIO.putStrLn err
                                Right res -> TSIO.putStrLn res >> execute ms newer_vm

defaultMaps :: Maps
defaultMaps = Maps defVar funMap opMap M.empty

compileAndRunFile :: FilePath -> CompileMode -> IO ()
compileAndRunFile f cm = compileAndRun f cm defaultMaps

parseEval :: Mode -> Maps -> StdGen -> TS.Text -> Either (MessageType, EvalState) (Complex Rational, EvalState)
parseEval md ms g x = evalExprS (parseString md x ms) ms g

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m defaultMaps

evalFile :: FilePath -> IO ()
evalFile f =  Calculator.interpret f Internal defaultMaps

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
      iD <- Web.Scotty.captureParam "id"
      liftIO $ BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") (B.toStrict . encode $ ((map (\(k, v) -> (k, (realPart v, imagPart v))) . M.toList $ defVar, [], opsToList opMap) :: ListTuple ))
      liftIO $ BS.writeFile ("log" ++ T.unpack iD ++ ".dat") "[]"
      Web.Scotty.html $ renderHtml
        $ H.html $ H.body $ do
          H.h1 $ H.toHtml ("Calculator" :: TS.Text)
          H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
            H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
          H.style $ H.toHtml . render $ getCss
    Web.Scotty.post "/clear/:id" $ do
      iD <- Web.Scotty.captureParam "id"
      Web.Scotty.redirect $ T.append "/" iD
    Web.Scotty.post "/:id" $ do
      iD <- Web.Scotty.captureParam "id"
      liftIO $ updateIDS (read . T.unpack $ iD :: Integer)
      fs <- Web.Scotty.formParam "foo"
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
          g1 <- liftIO $ readIORef ref
          let res = evalExprS t ms g1
          let txt = let (ress, EvalState mps ng) = either
                          Prelude.id
                          (\(r, mg) -> (MsgMsg . showComplex $ r, (maps . varmap %~ M.insert "_" r) mg))
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
    mapsToLists ms = (map (\(k, v) -> (k, (realPart v, imagPart v))) . M.toList $ (ms^.varmap), funsToList (ms^.funmap), opsToList (ms^.opmap))
    listsToMaps (a, b, c) = Maps (M.fromList . map (\(k, (vr, vi)) -> (k, vr:+vi)) $ a) (M.union funMap $ funsFromList b) (M.union opMap $ opsFromList c) M.empty
    unpackMsg (MsgMsg m) = m
    unpackMsg (ErrMsg e) = e
