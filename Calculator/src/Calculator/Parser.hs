{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Calculator.Parser
  ( parse
  ) where

import Calculator.Types
    ( preprocess,
      showRational,
      showT,
      unTOp,
      Assoc(R, L),
      Expr(..),
      Token(TLPar, TEnd, TOp, TIdent, TNumber, TComma, TRPar) )
import           Control.Arrow        (first)
import Control.Monad.Except
    ( Except, MonadTrans(lift), runExcept, MonadError(throwError) )
import Control.Monad.Reader
    ( ReaderT(runReaderT), MonadReader(ask) )
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import Data.List ( foldl' )

checkOps :: [Token] -> Either Text [Token]
checkOps t =
  if snd . foldl' f (TEnd, True) $ t
    then Right t
    else Left "Two operators in a row"
  where
    f (old, res) new = (new, res && not (is_op new && old == new))
    is_op (TOp "=") = False
    is_op (TOp _)   = True
    is_op _         = False

takeWithPrecedences :: Int -> Map Text Int -> [Token]
takeWithPrecedences n m = map (TOp . fst) . M.toList $ M.filter (== n) m

stringify :: [Token] -> Text
stringify [] = T.empty
stringify (x:xs) = str x <> stringify xs
  where
    str (TNumber n n1) = " " <> showRational n <> (if n1 == 0 then " " else " " <> showRational n1)
    str TLPar       = "("
    str TRPar       = ")"
    str (TIdent s)  = " " <> s <> " "
    str (TOp s)     = " " <> s <> " "
    str TComma      = ", "
    str _           = ""

type ParseReader = ReaderT (Map Text Int) (Except Text) Expr

parseOp :: Int -> [Token] -> ParseReader
parseOp 0 [TOp alias, TOp "=", TOp op] =
  return $ UDO alias (-1) L (Call op [Id "@x", Id "@y"])
parseOp 0 (TOp op:TLPar:TNumber p _:TComma:TNumber a _:TRPar:TOp "=":rest) =
  if null rest
    then throwError "Empty operator definition"
    else UDO
           op
           (floor p)
           (if a == 0
              then L
              else R) <$>
         parseOp 1 rest
parseOp 0 x@(TIdent _:TLPar:_) = do
  (a, b) <- lift $ breakPar (== TOp "=") x
  if null b
    then parseOp 1 x
    else do
      (name, args) <- lift $ parseFunDec a
      UDF name args <$> parseOp 1 (tail b)
parseOp 0 (TIdent s:TOp "=":rest) = Asgn s <$> parseOp 1 rest
parseOp 0 (TIdent s:TOp op:rest) | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&="] :: [Text]) = do
  a <- parseOp 1 rest
  return $ Asgn s (Call (T.init op) [Id s, a])
parseOp 0 s = parseOp 1 s
-- parseOp 3 (TOp "~":rest) = Call "comp" . (:[]) <$> parseOp 4 rest
-- parseOp 4 (TOp "+":rest) = parseOp 4 rest
-- parseOp 4 (TOp "-":rest) = UMinus <$> parseOp 4 rest
parseOp n (TOp op:rest)
  | op == "~" = Call "comp" . (:[]) <$> parseOp (n+1) rest
  | op == "+" = parseOp (n+1) rest
  | op == "-" = Call "-" . (:[]) <$> parseOp (n+1) rest
parseOp 15 s = parseToken s
parseOp l s = do
  m <- ask
  r <- lift $ breakPar3 (`elem` takeWithPrecedences l m) s
  case r of
    Nothing -> parseOp (l + 1) s
    Just ([], _, _) -> throwError "Empty first argument"
    Just (_, _, []) -> throwError "Empty second argument"
    Just (s1, op, s2) -> do
      a1 <- parseOp (l + 1) s1
      a2 <- parseOp l s2
      return $ Call (unTOp op) [a1, a2]

parseFunDec :: [Token] -> Except Text (Text, [Text])
parseFunDec [TIdent name, TLPar, TRPar] = return (name, [])
parseFunDec [TIdent name, TLPar, TIdent a, TRPar] = return (name, [a])
parseFunDec (TIdent name:TLPar:TIdent a:TComma:rest) = do
  x <- parseFunDec' rest
  return (name, a : x)
  where
    parseFunDec' y =
      case y of
        [TIdent _]            -> throwError "Missing bracket"
        [TIdent _, TComma]    -> throwError "Missing bracket"
        [TIdent n, TRPar]     -> return [n]
        (TIdent n:TComma:rst) -> (n :) <$> parseFunDec' rst
        x                     -> throwError $ "Syntax error in function declaration (mild): " <> stringify x
parseFunDec s = throwError $ "Syntax error in function declaration (severe): " <> stringify s

parseToken :: [Token] -> ParseReader
parseToken str =
  case str of
    []                       -> throwError "Empty"
    [TIdent s]               -> return $ Id s
    (TIdent name:TLPar:rest) -> Call name <$> parseFuncall rest
    [TNumber n ni]           -> return $ Number n ni
    (TLPar:rest)             -> Par <$> ps rest
    x                        -> throwError $ "Unknown token combination: " <> showT x
  where
    ps l = do
      (par, rest) <- lift $ takePar l
      if not (null rest)
        then throwError $ "Extra tokens: " <> stringify rest
        else (parseOp 1 . init) par

parseFuncall :: [Token] -> ReaderT (Map Text Int) (Except Text) [Expr]
parseFuncall [TRPar] = return []
parseFuncall s = do
  (s1, s2) <- lift $ breakPar (== TComma) s
  if s2 == [TComma, TRPar] || null s1
    then throwError "Missing function argument"
    else if null s2
           then (: []) <$>
                if length s1 > 1 && last s1 == TRPar
                  then parseOp 1 (init s1)
                  else parseOp 1 s1
           else (:) <$> parseOp 1 s1 <*> parseFuncall (tail s2)

takePar :: [Token] -> Except Text ([Token], [Token])
takePar = takePar' (1 :: Integer) []
  where
    takePar' 0 acc s = return (reverse acc, s)
    takePar' n _ [] = throwError $ "Parentheses mismatch: " <> showT n
    takePar' n acc (x:xs) =
      case x of
        TRPar -> tp (n - 1)
        TLPar -> tp (n + 1)
        _any  -> tp n
      where
        tp m = takePar' m (x : acc) xs

breakPar :: (Token -> Bool) -> [Token] -> Except Text ([Token], [Token])
breakPar _ [] = return ([], [])
breakPar p xs@(x:xs')
  | x == TLPar = do
    (a, b) <- takePar xs'
    (y, z) <- breakPar p b
    return ([x] ++ a ++ y, z)
  | p x = return ([], xs)
  | otherwise = first (x :) <$> breakPar p xs'

breakPar3 :: (Token -> Bool)
          -> [Token]
          -> Except Text (Maybe ([Token], Token, [Token]))
breakPar3 _ [] = return Nothing
breakPar3 p (x:xs)
  | x == TLPar = do
    (a, b) <- takePar xs
    r <- breakPar3 p b
    case r of
      Nothing            -> return Nothing
      Just (y, match, z) -> return (Just (x : a ++ y, match, z))
  | p x = return (Just ([], x, xs))
  | otherwise = fmap (\(a, b, c) -> (x : a, b, c)) <$> breakPar3 p xs

parse :: Map Text Int -> [Token] -> Either Text Expr
parse m s = do
  s1 <- checkOps s
  preprocess <$> runExcept (runReaderT (parseOp 0 s1) m)
