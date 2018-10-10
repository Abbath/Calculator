{-# LANGUAGE OverloadedStrings #-}
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
import           Calculator.Lexer
import qualified Calculator.MegaParser         as CMP
import           Calculator.Parser
import           Calculator.Types              (Assoc (..), Expr (..),
                                                ListTuple, preprocess,
                                                showRational)
import           Clay                          (render)
import           Control.Arrow                 (left, first, (***))
import           Control.Lens                  ((%~), (&), (.~), (^.), (^?), _1,
                                                _3)
import           Control.Monad.Reader
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
import           Web.Telegram.API.Bot

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Model = Model { getMaps :: Maps }
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply TS.Text  -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model (defVar, M.empty, opMap)
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate (Reply <$> Telegram.Bot.Simple.UpdateParser.text)

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Reply msg) model = model2 <# do
  replyText response
  pure NoAction
  where (response, model2) = (TS.pack *** Model) $ either 
          Prelude.id 
          (first showRational) 
          (parseEval Internal (getMaps model) (TS.unpack msg)) 

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
telegramSimple :: IO ()
telegramSimple = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run

data Mode = Internal | Megaparsec | AlexHappy deriving Show

parseString :: Mode -> String -> Maps -> Either String Expr
parseString m s ms = case m of
                       Megaparsec ->
                         left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (s++"\n"))
                       Internal ->
                         tokenize s >>= parse (getPriorities $ ms^._3)
                       AlexHappy -> Right $ preprocess . HP.parse . alexScanTokens $ s

evalExpr :: Either String Expr -> Maps -> Either (String, Maps) (Rational, Maps)
evalExpr t maps = case t of
             Left err -> Left (err, maps)
             Right r  -> eval maps r

opMap :: OpMap
opMap = M.fromList [("=", f 0 R)
  , ("==", f 1 L), ("<=", f 1 L), (">=", f 1 L), ("!=", f 1 L), ("<", f 1 L), (">", f 1 L)
  , ("+", f 2 L), ("-", f 2 L)
  , ("*", f 3 L), ("/", f 3 L), ("%", f 3 L)
  , ("^", f 4 R)]
  where f p a = ((p, a), Number 0)

getPrA :: OpMap -> Map String (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (\(s,(pa,_)) -> (s,pa)) lst
            in ps

getNames :: [String]
getNames = ["!=","%","*","+","-","/","<","<=","=","==",">",">=","^"
  ,"sin","cos","tan","asin","acos","atan","log","sqrt","exp","abs"
  ,"lt","gt","le","ge","eq","ne","if","df","gcd","lcm","div","mod","quot","rem","prat","quit"]

completionList :: Monad m => String -> m [Completion]
completionList s = return $ map (\x -> Completion {replacement = x, display = x, isFinished = False }) $ filter (isPrefixOf s) getNames

completeName :: Monad m => CompletionFunc m
completeName = completeWord Nothing " " completionList

parseEval :: Mode -> Maps -> String -> Either (String, Maps) (Rational, Maps)
parseEval md ms x = evalExpr (parseString md x ms) ms

loop :: Mode -> Maps -> IO ()
loop mode maps = runInputT (setComplete completeName $ defaultSettings { historyFile = Just "/home/dan/.mycalchist"}) (loop' mode maps)
  where
  loop' :: Mode -> Maps -> InputT IO ()
  loop' md ms = do
    input <- getInputLine "> "
    case input of
      Nothing -> return ()
      Just "quit" -> return ()
      Just x -> do
        case parseEval md ms x of
          Left (err,m) -> do
            liftIO $ putStrLn err
            loop' md m
          Right (r, m) -> do
            liftIO . putStrLn $ showRational r
            loop' md $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi", toRational (pi::Double)), ("e", toRational . exp $ (1::Double)), ("_",0.0)]

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m (defVar, M.empty, opMap)

updateIDS :: Integer -> IO ()
updateIDS i = do
    f <- BS.readFile "ids"
    let ids = read (BS.unpack f) :: [(Integer, Integer)]
    tm <- round `fmap` getPOSIXTime
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

bannedIPs :: [String]
bannedIPs = ["117.136.234.6", "117.135.250.134"]

theseFaggots :: Show a => a -> Bool
theseFaggots sa = let ss = show sa
                      (s1,rest) = break (=='.') ss
                      s2 = takeWhile (/='.') $ tail rest
                      n1 = read s1 :: Int
                      n2 = read s2 :: Int
                  in n1 == 117 && n2 >= 128 && n2 < 192

webLoop :: Int -> Mode -> IO ()
webLoop port mode = scotty port $ do
  middleware $ staticPolicy (noDots >-> addBase "Static/images") -- for future
  get "/" $ do
    req <- request
    let sa = remoteHost req
    liftIO $ print sa
    if takeWhile (/=':') (show sa) `elem` bannedIPs || theseFaggots sa
      then status $ Status 500 "Nope!"
      else do
        let x = liftIO (randomIO :: IO Integer)
        y <- x
        f <- liftIO $ findFile ["."] ("log" ++ show y ++ ".dat")
        if isJust f
          then redirect "/"
          else do
            liftIO $ updateIDS (abs y)
            redirect $ T.append "/" $ T.pack (show $ abs y)
  get "/webhook" $ do
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
  get "/favicon.ico" $ file "./Static/favicon.ico"
  get "/:id" $ do
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
        let ms = case (decode (B.fromStrict env) :: Maybe ListTuple) of
                   Just r  -> listsToMaps r
                   Nothing -> error "Cannot decode storage"
        let lg = fromMaybe (error "Cannot decode log") (decode (B.fromStrict rest) :: Maybe [(TS.Text, TS.Text)])
        let t = parseString mode (T.unpack fs) ms
        let res = evalExpr t ms
        let txt = case res of
                    Left (err, m) -> do
                      storeMaps storagename m
                      return $ (T.toStrict fs, TS.pack err) : lg
                    Right (r, m) -> do
                      storeMaps storagename (m & _1 %~ M.insert "_" r)
                      return $ (T.toStrict fs , TS.pack $ showRational r) : lg
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
      if not $ null u
        then do
          let Update { update_id = uid, message = msg, inline_query = iq} = head u
          let (tt,ch) = fromMaybe ("0",-1001040733151) (unpackTheMessage msg)
          let (qi, qq) = fromMaybe ("empty","0") (unpackQuery iq)
          let chat_maps = fromMaybe (defVar, M.empty, opMap) $ M.lookup (toInteger ch) maps
          let smm = TS.unpack tt
          if "/calc " `isPrefixOf` smm
          then do
            let sm = drop 6 smm
            let t = parseString mode sm chat_maps
            let res = evalExpr t chat_maps
            case res of
              Left (err,m) -> procRes ch (TS.pack err) m uid
              Right (r, m) -> procRes ch (TS.pack $ showRational r) (m & _1 %~ M.insert "_" r) uid
          else if qi /= "empty"
                then do
                  let t = parseString mode (TS.unpack qq) (defVar, M.empty, opMap)
                  let res = evalExpr t (defVar, M.empty, opMap)
                  case res of
                    Left (err, _) -> procQ qi qq (TS.pack err) uid
                    Right (r, _) -> procQ qi qq (TS.pack $ showRational r) uid
                else nextIter (uid+1)
        else nextIter (-1)
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
      case rs of
        Right Response { result = mr } -> printData mr
        Left err                       -> print err
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
