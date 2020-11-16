{-# LANGUAGE OverloadedStrings, TupleSections, OverloadedLists #-}
module Calculator (Mode(..), evalLoop, webLoop, defVar, opMap, telegramLoop, telegramSimple) where

import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Static
import qualified Network.Wreq                  as NW
import           Web.Scotty

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as TS
import           Data.Text.Encoding
import qualified Data.Text.IO                  as TSIO
import qualified Data.Text.Lazy                as T

import           Calculator.AlexLexer
import           Calculator.Css
import           Calculator.Evaluator
import qualified Calculator.HappyParser        as HP
-- import           Calculator.Lexer
import qualified Calculator.MegaParser         as CMP
import           Calculator.Parser
import           Calculator.HomebrewLexer
import           Calculator.Types              (Assoc (..), Expr (..),
                                                ListTuple, preprocess,
                                                showRational, showT)
import           Clay                          (render)
import           Control.Arrow                 (left, first, second)
import           Control.Lens                  ((%~), (&), (.~), (^.), (^?), _1,
                                                _3)
import           Control.Monad.Reader
import           Control.Monad.State           
import           Control.Monad.Except          
import           Data.Aeson                    (decode, encode)
import           Data.Aeson.Lens
import           Data.List                     (isPrefixOf)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe, isJust, isNothing)
import           Data.Time.Clock.POSIX
import           Network.HTTP.Client           (Manager, newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Random
import qualified Text.Megaparsec               as MP
import           Text.Read                     (readMaybe)
import           Web.Telegram.API.Bot

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser
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
                       AlexHappy -> Right $ preprocess . HP.parse . alexScanTokens $ TS.unpack s

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
  ,"sin","cos","tan","asin","acos","atan","log","sqrt","exp","abs","xor","not","int","df"
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
webLoop port mode = scotty port $ do
  middleware $ staticPolicy (noDots >-> addBase "Static/images") -- for future
  Web.Scotty.get "/" $ do
    req <- request
    let sa = remoteHost req
    liftIO $ print sa
    do
      let x = liftIO (randomIO :: IO Integer)
      y <- x
      f <- liftIO $ findFile ["."] ("log" ++ show y ++ ".dat")
      if isJust f
        then redirect "/"
        else do
          liftIO $ updateIDS (abs y)
          redirect $ T.append "/" $ T.pack (show $ abs y)
  Web.Scotty.get "/webhook" $ do
    vt <- param "hub.verify_token"
    c <- param "hub.challenge"
    if vt == m_token
      then Web.Scotty.text (c :: T.Text)
      else json ("Error!" :: TS.Text)
  post "/webhook" $ do
    b <- body
    let msgs = b ^? key "entry" . nth 0 . key "messaging"
    mapM_ (\m -> do
              let msg = fromMaybe "Hello" $ m ^? key "message" . key "text" . _String
              let sender = fromMaybe "" $ m ^? key "sender" . key "id" . _String
              let opts = NW.defaults & NW.param "qs" .~ [TS.concat ["{access_token : \"", m_token,"\"}"]]
              let resp = TS.concat ["{recipient : {id:\"", sender,"\"}", ", messageData: {text : \"", msg ,"\"}}"]
              r <- liftIO $ NW.postWith opts "https://graph.facebook.com/v2.6/me/messages" $ encodeUtf8 resp
              liftIO $ print r
              return ()) msgs
    status $ Status 200 "Ok"
  Web.Scotty.get "/favicon.ico" $ file "./Static/favicon.ico"
  Web.Scotty.get "/:id" $ do
    iD <- param "id"
    liftIO $ BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") (B.toStrict . encode $ ( (M.toList defVar, [], M.toList opMap) :: ListTuple ))
    liftIO $ BS.writeFile ("log" ++ T.unpack iD ++ ".dat") "[]"
    html $ renderHtml
      $ H.html $ H.body $ do
        H.h1 $ H.toHtml ("Calculator" :: TS.Text)
        H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
          H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
        H.style $ H.toHtml . render $ getCss
  post "/clear/:id" $ do
    iD <- param "id"
    redirect $ T.append "/" iD
  post "/:id" $ do
    iD <- param "id"
    liftIO $ updateIDS (read . T.unpack $ iD :: Integer)
    fs <- param "foo"
    f1 <- liftIO $ findFile ["."] ("storage" ++ T.unpack iD ++ ".dat")
    f2 <- liftIO $ findFile ["."] ("log" ++ T.unpack iD ++ ".dat")
    if isNothing f1 || isNothing f2
      then redirect "/"
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
        html $ renderHtml
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
    m_token = "EAADXPmCvIzUBAJsNSv4hbrFdvCXhhT5tpHoxbdW3YVjgWdjdkiudNjWLSo73ETD7nqyaneCutffik98dYE0mRImZCZB6ZBiZA87GKXAjwuGmRZCeXUxZA8pLHlF64evFiY1WFTeZALazOI3NxUXOwZAQTqkeuI7w5elrjZA8Shrin2zeds" :: TS.Text

telegramLoop :: Mode -> IO ()
telegramLoop mode = do
  manager <- newManager tlsManagerSettings
  telegramLoop' mode M.empty manager (-1)

telegramLoop' :: Mode -> Map Integer Maps -> Manager -> Int -> IO ()
telegramLoop' mode maps manager n = do
  tok <- token
  updates <- getUpdates tok (Just n) (Just 10) Nothing manager
  case updates of
    Right Response { result = u } ->
      case u of 
        (x:_) -> do
          let Update { update_id = uid, message = msg, inline_query = iq} = x
          let (tt,ch) = fromMaybe ("0",-1001040733151) (unpackTheMessage msg)
          let (qi, qq) = fromMaybe ("empty","0") (unpackQuery iq)
          let chat_maps = fromMaybe (defVar, M.empty, opMap) $ M.lookup (toInteger ch) maps
          let smm = tt
          if "/calc " `TS.isPrefixOf` smm
          then do
            let sm = TS.drop 6 smm
            let t = parseString mode sm chat_maps
            let res = evalExprS t chat_maps
            case res of
              Left  (err, m) -> procRes ch err m uid
              Right (r, m)   -> procRes ch (showRational r) (m & _1 %~ M.insert "_" r) uid
          else if qi /= "empty"
                then do
                  let t = parseString mode qq (defVar, M.empty, opMap)
                  let res = evalExprS t (defVar, M.empty, opMap)
                  procQ qi qq (either fst (showRational . fst) res) uid
                else nextIter (uid+1)
        _ -> nextIter (-1)
    Left err -> do
      print err
      nextIter (-1)
  where
    nextIter = telegramLoop' mode maps manager
    unpackQuery q = do
      iq <- q
      return (query_id iq, query_query iq)
    unpackTheMessage m = do
      mm <- m
      t <- Web.Telegram.API.Bot.text mm
      let ch = chat mm
      return (t, fromIntegral $ chat_id ch)
    token = do
      ep <- getExecutablePath
      t <- TSIO.readFile $ takeDirectory ep </> "token"
      return $ Token t
    printData mr = do
      print (message_id mr)
      print (Web.Telegram.API.Bot.text mr)
      print (maybe 0 user_id . from $ mr)
      print (chat_id . chat $ mr)
    procRes ch s m uid = do
      t <- token
      rs <- sendMessage t (SendMessageRequest (ChatId ch) s Nothing Nothing Nothing Nothing Nothing) manager
      either print (\Response { result = mr } -> printData mr) rs
      telegramLoop' mode (M.insert (toInteger ch) m maps) manager (uid+1)
    ir txt ii = InlineQueryResultArticle ii (Just txt) (Just $ InputTextMessageContent txt Nothing Nothing) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    procQ qi qq s uid = do
      x <- randomIO :: IO Integer
      t <- token
      rs <- answerInlineQuery t (AnswerInlineQueryRequest qi [ir (TS.concat [qq, " = ", s]) (TS.pack $ show x)] Nothing Nothing Nothing Nothing Nothing) manager
      case rs of
           Right Response { result = b} -> print b
           Left err                     -> print err
      nextIter (uid+1)
