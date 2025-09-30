{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Calculator.Generator where

import Calculator.Types (
  Expr (Asgn, Call, Id, Number, Par),
  showComplex,
  showT,
 )
import Control.Lens ((%~), (^.), _1, _2, _3)
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  runExceptT,
 )
import Control.Monad.State (
  MonadState (get),
  State,
  execState,
  gets,
  modify,
 )
import Data.Complex
import Data.Text (Text)
import Data.Text qualified as T

data Tac
  = TacOp Text Text Text Text
  | TacFun Text Text Text

instance Show Tac where
  show (TacOp var a op b) = T.unpack $ var <> " := " <> a <> " " <> op <> " " <> b
  show (TacFun var fun a) = T.unpack $ var <> " := " <> fun <> "(" <> a <> ")"

type ResultG = ExceptT Text (State (Int, Int, [Tac]))

isFinal :: Expr -> Bool
isFinal (Number _ _) = True
isFinal (Id _) = True
isFinal _ = False

extractFinal :: Expr -> Text
extractFinal (Number a _) = showT @Double (fromRational a)
extractFinal (Id a) = a
extractFinal _ = error "Extraction is impossible"

generate' :: Expr -> ResultG ()
generate' expr = do
  (n, ln, acc) <- get
  case expr of
    e | isFinal e -> do
      modify (_3 %~ const [TacOp (t n) (extractFinal e) "" ""])
      do_a_barrel_roll n
    (Asgn var (Call fun [a])) | isFinal a -> modify (_3 %~ (:) (TacFun var fun (extractFinal a)))
    (Asgn var (Call fun [a])) -> do
      generate' a
      ln_ <- gets (^. _2)
      modify (_3 %~ (:) (TacFun var fun (t ln_)))
    (Call fun [a]) | isFinal a -> do
      modify (_3 %~ (:) (TacFun (t n) fun (extractFinal a)))
      do_a_barrel_roll n
    (Call fun [a]) -> do
      generate' a
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacFun (t n_) fun (t ln_)))
      do_a_barrel_roll n_
    (Asgn var (Call op [a, b])) | isFinal a && isFinal b -> modify (_3 %~ (:) (TacOp var (extractFinal a) op (extractFinal b)))
    (Asgn var (Call op [a, b])) | isFinal b && not (isFinal a) -> do
      generate' a
      ln_ <- gets (^. _2)
      modify (_3 %~ (:) (TacOp var (t ln_) op (extractFinal b)))
    (Asgn var (Call op [a, b])) | isFinal a && not (isFinal b) -> do
      generate' b
      ln_ <- gets (^. _2)
      modify (_3 %~ (:) (TacOp var (extractFinal a) op (t ln_)))
    (Asgn var (Call op [a, b])) -> do
      generate' a
      ln_a <- gets (^. _2)
      generate' b
      ln_b <- gets (^. _2)
      modify (_3 %~ (:) (TacOp var (t ln_a) op (t ln_b)))
    (Call op [a, b]) | isFinal a && isFinal b -> do
      modify (_3 %~ (:) (TacOp (t n) (extractFinal a) op (extractFinal b)))
      do_a_barrel_roll n
    (Call op [a, b]) | isFinal a && not (isFinal b) -> do
      generate' b
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (extractFinal a) op (t ln_)))
      do_a_barrel_roll n_
    (Call op [a, b]) | isFinal b && not (isFinal a) -> do
      generate' a
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (t ln_) op (extractFinal b)))
      do_a_barrel_roll n_
    (Call op [a, b]) -> do
      generate' a
      ln_a <- gets (^. _2)
      generate' b
      (n_, ln_b, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (t ln_a) op (t ln_b)))
    (Par e) -> do
      generate' e
    _ -> throwError "Not supported"
  pure ()
 where
  t = ("t" <>) . showT
  do_a_barrel_roll n = do
    modify (_1 %~ (+ 1))
    modify (_2 %~ const n)

generateTac :: Expr -> [Tac]
generateTac e = let a = runExceptT (generate' e) in reverse $ execState a (0, 0, []) ^. _3

type QbeState = ExceptT Text (State Text)

generateQbe :: Expr -> QbeState ()
generateQbe expr = case expr of
  (Number a b) -> modify (<> ("d_" <> showComplex (a :+ b)))
  (Call "+" [a, b]) -> do
    modify (<> "add ")
    generateQbe a
    generateQbe b
  _ -> throwError "Not supported"
