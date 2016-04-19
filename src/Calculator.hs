{-# LANGUAGE OverloadedStrings #-}
module Calculator (Mode(..), evalLoop, webLoop, defVar, opMap, telegramLoop) where

import Web.Scotty
import Network.Wai
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import Data.Map.Strict (Map)
import Control.Lens (_1, _3, (^.), (&), (%~))
import qualified Data.Map.Strict as M
import Calculator.Types (Expr(..), Assoc(..), ListTuple, showRational)
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator
import Calculator.Css
import qualified Text.Megaparsec as MP
import qualified Calculator.MegaParser as CMP
import Control.Monad.Reader
import System.Console.Haskeline
import Control.Arrow (left)
import Data.List (isPrefixOf)
import Clay (render)
import Data.Aeson (encode, decode)
import System.Random
import System.Directory
import Data.Time.Clock.POSIX
import Data.Maybe (fromMaybe, isJust, isNothing)
import Web.Telegram.API.Bot

data Mode = Internal | Megaparsec deriving Show

parseString :: Mode -> String -> Maps -> Either String Expr
parseString m s ms = case m of
                       Megaparsec ->
                         left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (s++"\n"))
                       Internal ->
                         tokenize s >>= parse (getPriorities $ ms^._3)

evalExpr :: Either String Expr -> Maps -> Either (String, Maps) (Rational, Maps)                         
evalExpr t maps = case t of 
             Left err -> Left (err, maps)
             Right r -> eval maps r
                         
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
        let t = parseString md x ms
        let res = evalExpr t ms
        case res of
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
                   Just r -> listsToMaps r
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
    
telegramLoop :: Mode -> IO ()
telegramLoop mode = telegramLoop' mode M.empty (-1)
    
telegramLoop' :: Mode -> Map Int Maps -> Int -> IO ()
telegramLoop' mode maps n = do
  updates <- getUpdates token (Just n) (Just 10) Nothing
  case updates of 
    Right UpdatesResponse { update_result = u } -> 
      if not $ null u
        then do
          let Update { update_id = uid, message = msg} = head u
          let (tt,ch) = fromMaybe ("0",-1001040733151) (unpackTheMessage msg)
          let chat_maps = fromMaybe (defVar, M.empty, opMap) $ M.lookup ch maps
          let smm = TS.unpack tt
          if "/calc " `isPrefixOf` smm 
          then do
            let sm = drop 6 smm 
            let t = parseString mode sm chat_maps
            let res = evalExpr t chat_maps 
            case res of
              Left (err,m) -> procRes ch (TS.pack err) m uid
              Right (r, m) -> procRes ch (TS.pack $ showRational r) (m & _1 %~ M.insert "_" r) uid
          else nextIter (uid+1)
        else nextIter (-1)
    Left err -> do 
      print err
      nextIter (-1)
  where 
    nextIter = telegramLoop' mode maps  
    unpackTheMessage m = do
      mm <- m
      t <- Web.Telegram.API.Bot.text mm
      let ch = chat mm
      return (t, chat_id ch)
    token = Token "bot202491437:AAHMzKzAmcMaibK5O2fanEJbdb71S4IiOzA"
    printData mr = do
      print (message_id mr)
      print (Web.Telegram.API.Bot.text mr)
      print (user_id . from $ mr)
      print (chat_id . chat $ mr)  
    procRes ch s m uid = do 
      rs <- sendMessage token (SendMessageRequest (TS.pack $ show ch) s Nothing Nothing Nothing Nothing)  
      case rs of 
        Right MessageResponse { message_result = mr } -> printData mr  
        Left err -> print err  
      telegramLoop' mode (M.insert ch m maps) (uid+1)  