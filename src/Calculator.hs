{-# LANGUAGE OverloadedStrings #-}
module Calculator (Mode(..), evalLoop, webLoop) where

import Web.Scotty

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO

import Data.Map.Strict (Map)
import Control.Lens (_1, _3, (^.), (&), (%~))
import qualified Data.Map.Strict as M
import Calculator.Types (Expr(..), Assoc(..))
import Calculator.Lexer
import Calculator.Parser
import Calculator.Evaluator
import qualified Text.Megaparsec as MP
import qualified Calculator.MegaParser as CMP
import Control.Monad.Reader
import System.Console.Haskeline
import Control.Arrow (left)
import Data.List (isPrefixOf)

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
  ,"lt","gt","le","ge","eq","ne","if","df","quit"]

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
            liftIO . print $ (fromRational r :: Double)
            loop' md $ m & _1 %~ M.insert "_" r

defVar :: VarMap
defVar = M.fromList [("pi", toRational (pi::Double)), ("e", toRational . exp $ (1::Double)), ("_",0.0)]

evalLoop :: Mode -> IO ()
evalLoop m = Calculator.loop m (defVar, M.empty, opMap)

webLoop :: Mode -> IO ()
webLoop mode = scotty 3000 $ do
    get "/" $ do
        liftIO $ TIO.writeFile "storage.dat" (TS.pack ("(" ++ show defVar ++ ",fromList []," ++ show opMap ++ ")")) 
        liftIO $ TIO.writeFile "log.dat" ""         
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/" $ do
                        H.input H.! type_ "input" H.! name "foo"
  
    post "/clear" $ do
        liftIO $ TIO.writeFile "storage.dat" (TS.pack ("(" ++ show defVar ++ ",fromList []," ++ show opMap ++ ")")) 
        liftIO $ TIO.writeFile "log.dat" ""
        html $ "Ok"
        redirect "/"
    post "/" $ do
        fs <- param "foo"
        rest <- liftIO $ TIO.readFile "log.dat"
        env <- liftIO $ TIO.readFile "storage.dat"
        let ms = read (TS.unpack env) :: Maps
        let t = case mode of
                  Megaparsec ->
                    left show (MP.runParser (runReaderT CMP.parser (getPrA $ ms^._3)) "" ((T.unpack fs) ++ "\n")) >>= eval ms
                  Internal ->
                    tokenize (T.unpack fs) >>= parse (getPriorities $ ms^._3) >>= eval ms
        let txt = case t of
                    Left err -> return (T.toStrict $ T.append (T.append (T.pack err) "\n") (T.fromStrict rest))
                    Right (r, m) -> do
                      TIO.writeFile "storage.dat" . TS.pack . show $ m & _1 %~ M.insert "_" r            
                      return (T.toStrict $ T.append (T.append (T.pack . show $ (fromRational r :: Double)) "\n") (T.fromStrict rest))
        rtxt <- liftIO txt 
        liftIO $ TIO.writeFile "log.dat" rtxt
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/" $ do
                        H.input H.! type_ "input" H.! name "foo"
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/clear" $ do
                        H.input H.! type_ "submit" H.! value "Clear history"      
                    H.ul $ mapM_ (H.li . H.toHtml) (TS.lines rtxt)

