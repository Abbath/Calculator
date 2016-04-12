{-# LANGUAGE OverloadedStrings #-}
module Calculator (Mode(..), evalLoop, webLoop) where

import Web.Scotty

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
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (x++"\n")) >>= eval ms
                  Internal ->
                    tokenize x >>= parse (getPriorities $ ms^._3) >>= eval ms
        case t of
          Left err -> do
            liftIO $ putStrLn err
            loop' md ms
          Right (r, m) -> do
            liftIO . putStrLn $ if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)
            loop' md $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi", toRational (pi::Double)), ("e", toRational . exp $ (1::Double)), ("_",0.0)]

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m (defVar, M.empty, opMap)

webLoop :: Int -> Mode -> IO ()
webLoop port mode = scotty port $ do
    get "/" $ do
        liftIO $ B.writeFile "storage.dat" (encode $ ( (M.toList defVar, [], M.toList opMap) :: ListTuple ))
        liftIO $ BS.writeFile "log.dat" "[]"
        html $ renderHtml
             $ H.html $
                H.body $ do
                    H.h1 $ H.toHtml (TS.pack "Calculator")
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/" $
                        H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
                    H.style $
                      H.toHtml . render $ getCss 
    post "/clear" $ do
        redirect "/"
    post "/" $ do
        fs <- param "foo"
        rest <- liftIO $ BS.readFile $ "log.dat"
        env <- liftIO $ B.readFile "storage.dat"
        let ms = case (decode env :: Maybe ListTuple) of 
                  Just r -> listsToMaps r
                  Nothing -> error "Cannot decode storage"
        let lg = case decode (B.fromStrict rest) :: Maybe [(TS.Text, TS.Text)] of
                   Just r -> r
                   Nothing -> error "Cannot decode log"
        let t = case mode of
                  Megaparsec ->
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" (T.unpack fs ++ "\n")) >>= eval ms
                  Internal ->
                    tokenize (T.unpack fs) >>= parse (getPriorities $ ms^._3) >>= eval ms
        let txt = case t of
                    Left err -> return  $ (T.toStrict fs, TS.pack err) : lg
                    Right (r, m) -> do
                      B.writeFile "storage.dat" . encode . mapsToLists $ (m & _1 %~ M.insert "_" r) 
                      return $ (T.toStrict fs , TS.pack $ if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)) : lg
        rtxt <- liftIO txt
        liftIO $ BS.writeFile "log.dat" . B.toStrict . encode $ rtxt
        html $ renderHtml
             $ H.html $
                H.body $ do
                    H.h1 $ H.toHtml ("Calculator" :: TS.Text)
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/" $
                        H.input H.! type_ "input" H.! name "foo" H.! autofocus "autofocus"
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/clear" $
                        H.input H.! type_ "submit" H.! value "Clear history"
                    H.table $ mapM_ (\(x,y) -> H.tr $ (H.td . H.toHtml $ x) >> (H.td . H.toHtml $ y)) rtxt
                    H.style $
                      H.toHtml . render $ postCss
    where 
        mapsToLists = \(a,b,c) -> (M.toList a, M.toList b, M.toList c)
        listsToMaps = \(a,b,c) -> (M.fromList a, M.fromList b, M.fromList c)
    
        