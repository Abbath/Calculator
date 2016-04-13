{-# LANGUAGE OverloadedStrings #-}
module Calculator (Mode(..), evalLoop, webLoop) where

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
import Calculator.Types (Expr(..), Assoc(..), ListTuple)
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
import Data.Ratio (numerator, denominator)
import Clay (render)
import Data.Aeson (encode, decode)
import System.Random
import System.Directory
import Data.Time.Clock.POSIX

data Mode = Internal | Megaparsec deriving Show

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
        let t = case md of
                  Megaparsec ->
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (x++"\n"))
                  Internal ->
                    tokenize x >>= parse (getPriorities $ ms^._3)
        let res = case t of 
                    Left err -> Left (err, ms)
                    Right r -> eval ms r 
        case res of
          Left (err,m) -> do
            liftIO $ putStrLn err
            loop' md m
          Right (r, m) -> do
            liftIO . putStrLn $ if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)
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
             removeFile ("storage" ++ show ff ++ ".dat")
             removeFile ("log" ++ show ff ++ ".dat")) $ filter (\(a,_) -> tm-a > 60*60) ids
    if i `elem` map snd ids 
       then BS.writeFile "ids" $ BS.pack $ show $ map (\(a,b) -> if b == i then (tm,i) else (a,b)) ids
       else BS.writeFile "ids" $ BS.pack $ show $ (tm,i) : filter  (\(a,_) -> tm - a < 60*60) ids
       
bannedIPs :: [String]
bannedIPs = ["127.0.0.1", "117.136.234.6"]
       
webLoop :: Int -> Mode -> IO ()
webLoop port mode = scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for future
    get "/" $ do 
        req <- request
        let sa = remoteHost req
        liftIO $ print sa
        if (fst $ break (==':') $ show sa) `elem` bannedIPs
           then status $ Status 500 "Nope!"
           else do
             let x = liftIO $ (randomIO :: IO Integer)
             y <- x
             liftIO $ updateIDS (abs y)
             redirect $ T.append "/" $ T.pack (show $ abs y)
    get "/:id" $ do
        iD <- param "id"
        liftIO $ BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") (B.toStrict . encode $ ( (M.toList defVar, [], M.toList opMap) :: ListTuple ))
        liftIO $ BS.writeFile ("log" ++ T.unpack iD ++ ".dat") "[]"
        html $ renderHtml
             $ H.html $
                H.body $ do
                    H.h1 $ H.toHtml ("Calculator" :: TS.Text)
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
                        H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
                    H.style $
                      H.toHtml . render $ getCss 
    get "/favicon.ico" $ file "Static/favicon.ico"
    post "/clear/:id" $ do
        iD <- param "id"
        redirect $ T.append "/" iD
    post "/:id" $ do
        iD <- param "id"
        liftIO $ updateIDS (read . T.unpack $ iD :: Integer) 
        fs <- param "foo"
        rest <- liftIO $ BS.readFile $ ("log" ++ T.unpack iD ++ ".dat")
        env <- liftIO $ BS.readFile ("storage" ++ T.unpack iD ++ ".dat")
        let ms = case (decode (B.fromStrict env) :: Maybe ListTuple) of 
                  Just r -> listsToMaps r
                  Nothing -> error "Cannot decode storage"
        let lg = case decode (B.fromStrict rest) :: Maybe [(TS.Text, TS.Text)] of
                   Just r -> r
                   Nothing -> error "Cannot decode log"
        let t = case mode of
                  Megaparsec ->
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (T.unpack fs ++ "\n")) 
                  Internal ->
                    tokenize (T.unpack fs) >>= parse (getPriorities $ ms^._3)
        let res = case t of 
                    Left err -> Left (err, ms)
                    Right r -> eval ms r 
        let txt = case res of
                    Left (err, m) -> do
                      BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") . B.toStrict . encode . mapsToLists $ m
                      return  $ (T.toStrict fs, TS.pack err) : lg
                    Right (r, m) -> do
                      BS.writeFile ("storage" ++ T.unpack iD ++ ".dat") . B.toStrict . encode . mapsToLists $ (m & _1 %~ M.insert "_" r) 
                      return $ (T.toStrict fs , TS.pack $ if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)) : lg
        rtxt <- liftIO txt
        liftIO $ BS.writeFile ("log" ++ T.unpack iD ++ ".dat") . B.toStrict . encode $ rtxt
        html $ renderHtml
             $ H.html $
                H.body $ do
                    H.h1 $ H.toHtml ("Calculator" :: TS.Text)
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/" iD) $
                        H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action (H.toValue $ T.append "/clear/" iD) $
                        H.input H.! type_ "submit" H.! value "Clear history"
                    H.table $ mapM_ (\(x,y) -> H.tr $ (H.td . H.toHtml $ x) >> (H.td . H.toHtml $ y)) rtxt
                    H.style $
                      H.toHtml . render $ postCss
    where 
        mapsToLists = \(a,b,c) -> (M.toList a, M.toList b, M.toList c)
        listsToMaps = \(a,b,c) -> (M.fromList a, M.fromList b, M.fromList c)
    
        