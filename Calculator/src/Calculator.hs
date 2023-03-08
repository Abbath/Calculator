{-# LANGUAGE OverloadedStrings, TupleSections, OverloadedLists #-}
module Calculator (Mode(..), evalLoop, webLoop, defVar, opMap, telegramSimple) where

import Network.Wai ( Request(remoteHost) )
import Network.Wai.Middleware.Static
    ( (>->), addBase, noDots, staticPolicy )
import Web.Scotty
    ( file,
      get,
      html,
      middleware,
      param,
      post,
      redirect,
      request,
      scotty )

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import Text.Blaze.Html5.Attributes
    ( action, autofocus, enctype, method, name, type_, value )

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as TS
import qualified Data.Text.IO                  as TSIO
import qualified Data.Text.Lazy                as T

import Calculator.AlexLexer ( alexScanTokens )
import Calculator.Css ( getCss, postCss )
import Calculator.Evaluator
    ( Maps, OpMap, VarMap, FunMap, getPriorities, evalS )
import qualified Calculator.HappyParser        as HP
-- import           Calculator.Lexer
import qualified Calculator.MegaParser         as CMP
import Calculator.Parser ( parse )
import Calculator.HomebrewLexer ( tloop )
import           Calculator.Types              (Assoc (..), Expr (..),
                                                ListTuple, preprocess,
                                                showRational, showT)
import           Clay                          (render)
import           Control.Arrow                 (left, first, second)
import           Control.Lens                  ((%~), (&), (^.), _1, _3)
import Control.Monad.Reader
    ( MonadIO(liftIO), when, ReaderT(runReaderT) )
import Control.Monad.State ( runState )           
import Control.Monad.Except ( runExceptT )          
import           Data.Aeson                    (decode, encode)
import           Data.List                     (isPrefixOf)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe, isJust, isNothing)
import Data.Time.Clock.POSIX ( getPOSIXTime )

import System.Console.Haskeline
    ( defaultSettings,
      getInputLine,
      completeWord,
      runInputT,
      setComplete,
      Completion(..),
      CompletionFunc,
      InputT,
      Settings(historyFile) )
import System.Directory ( findFile, getHomeDirectory, removeFile )
import System.Random ( randomIO )
import qualified Text.Megaparsec               as MP
import           Text.Read                     (readMaybe)

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

newtype Model = Model {getMaps :: Maps}

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply !TS.Text  -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: Mode -> BotApp Model Action
bot mode = BotApp
  { botInitialModel = Model (defVar, M.empty, opMap)
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
  replyText response
  pure NoAction
  where (response, model2) = second Model $ either 
          Prelude.id 
          (first showRational) 
          (parseEval mode (getMaps model) msg) 

-- | Run bot with a given 'Telegram.Token'.
run :: Mode -> Telegram.Token -> IO ()
run mode token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId (bot mode)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: Mode -> IO ()
telegramSimple mode = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run mode

data Mode = Internal | Megaparsec | AlexHappy deriving Show

parseString :: Mode -> TS.Text -> Maps -> Either TS.Text Expr
parseString m s ms = case m of
                       Megaparsec ->
                         left showT (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (s <> "\n"))
                       Internal ->
                         tloop s >>= parse (getPriorities $ ms^._3)
                       AlexHappy -> Right $ preprocess . HP.parse . Calculator.AlexLexer.alexScanTokens $ TS.unpack s

evalExprS :: Either TS.Text Expr -> Maps -> Either (TS.Text, Maps) (Rational, Maps)
evalExprS t maps = either (Left . (,maps)) ((\(r, s) -> either (Left . (,s)) (Right . (,s)) r) . getShit) t
  where getShit e = let a = runExceptT (evalS e) in runState a maps

-- evalExpr :: Either String Expr -> Maps -> Either (String, Maps) (Rational, Maps)
-- evalExpr t maps = either (Left . (,maps)) (eval  maps) t

opMap :: OpMap
opMap = [("=", f 0 R)
  , ("==", f 1 L), ("<=", f 1 L), (">=", f 1 L), ("!=", f 1 L), ("<", f 1 L), (">", f 1 L)
  , ("+", f 2 L), ("-", f 2 L)
  , ("*", f 3 L), ("/", f 3 L), ("%", f 3 L)
  , ("^", f 4 R)
  , ("|", f 5 R)
  , ("&", f 6 R)]
  where f p a = ((p, a), Number 0)

getPrA :: OpMap -> Map TS.Text (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (second fst) lst
            in ps

getNames :: [String]
getNames = ["!=","%","*","+","-","/","<","<=","=","==",">",">=","^","&","|"
  ,"sin","cos","tan","asin","acos","atan","log","sqrt","exp","abs","xor","not","int","df","hex"
  ,"lt","gt","le","ge","eq","ne","if","df","gcd","lcm","div","mod","quot","rem","prat","quit"]

completionList :: Monad m => String -> m [Completion]
completionList s = return $ map (\x -> Completion {replacement = x, display = x, isFinished = False }) $ filter (isPrefixOf s) getNames

completeName :: Monad m => CompletionFunc m
completeName = completeWord Nothing " " completionList

parseEval :: Mode -> Maps -> TS.Text -> Either (TS.Text, Maps) (Rational, Maps)
parseEval md ms x = evalExprS (parseString md x ms) ms

loop :: Mode -> Maps -> IO ()
loop mode maps = do
    hd <- getHomeDirectory
    x <- CE.try $ runInputT 
      (setComplete completeName $
       defaultSettings { historyFile = Just (hd ++ "/.mycalchist")}) 
       (loop' mode maps) :: IO (Either CE.ErrorCall ())
    case x of
      Left (CE.ErrorCall s) -> do
        putStrLn s
        Calculator.loop mode maps
      Right _ -> return ()
  where
  loop' :: Mode -> Maps -> InputT IO ()
  loop' md ms = do
    input <- getInputLine "> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just x -> do
          let y = parseEval md ms (TS.pack x)
          case y of
            Left (err,m) -> do
              liftIO $ TSIO.putStrLn err
              loop' md m
            Right (r, m) -> do
              liftIO . TSIO.putStrLn $ showRational r
              loop' md $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = [("m.pi", toRational (pi::Double)), ("m.e", toRational . exp $ (1::Double)), ("m.phi", toRational ((1+sqrt 5)/2::Double)), ("_",0.0)]

funMap :: FunMap
funMap = [(("not", 1), (["x"], FunCall "if" [Id "x", Number 0, Number 1]))]

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m (defVar, funMap, opMap)

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
    if i `elem` map snd ids
       then BS.writeFile "ids" $ BS.pack $ show $ map (\(a,b) -> if b == i then (tm,i) else (a,b)) ids
       else BS.writeFile "ids" $ BS.pack $ show $ (tm,i) : filter  (\(a,_) -> tm - a < 60*60) ids

webLoop :: Int -> Mode -> IO ()
webLoop port mode = Web.Scotty.scotty port $ do
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
    liftIO $ BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") (B.toStrict . encode $ ( (M.toList defVar, [], M.toList opMap) :: ListTuple ))
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
        let res = evalExprS t ms
        let txt = let (ress, mps) = either 
                        Prelude.id 
                        (\(r, m) -> (showRational r, m & _1 %~ M.insert "_" r)) 
                        res
                  in do
                      storeMaps storagename mps
                      return $ (fs, ress) : lg
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
    mapsToLists (a, b, c) = (M.toList a, M.toList b, M.toList c)
    listsToMaps (a, b, c) = (M.fromList a, M.fromList b, M.fromList c)
