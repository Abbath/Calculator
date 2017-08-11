{-# OPTIONS_GHC -w #-}
module Calculator.HappyParser where
import Calculator.AlexLexer
import Calculator.Types (Token(..), Expr(..), Assoc(..))
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (12) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (17) = happyShift action_2
action_0 (18) = happyShift action_10
action_0 (19) = happyShift action_11
action_0 (20) = happyShift action_12
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 _ = happyFail

action_1 (17) = happyShift action_2
action_1 _ = happyFail

action_2 (10) = happyShift action_20
action_2 _ = happyFail

action_3 (21) = happyAccept
action_3 _ = happyFail

action_4 (11) = happyShift action_19
action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 _ = happyReduce_10

action_7 _ = happyReduce_11

action_8 (9) = happyShift action_6
action_8 (10) = happyShift action_7
action_8 (12) = happyShift action_8
action_8 (16) = happyShift action_9
action_8 (20) = happyShift action_12
action_8 (6) = happyGoto action_18
action_8 _ = happyFail

action_9 (9) = happyShift action_6
action_9 (10) = happyShift action_7
action_9 (12) = happyShift action_8
action_9 (16) = happyShift action_9
action_9 (20) = happyShift action_12
action_9 (6) = happyGoto action_17
action_9 _ = happyFail

action_10 (20) = happyShift action_16
action_10 _ = happyFail

action_11 (10) = happyShift action_15
action_11 _ = happyFail

action_12 (9) = happyShift action_6
action_12 (10) = happyShift action_7
action_12 (12) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (20) = happyShift action_12
action_12 (6) = happyGoto action_13
action_12 (8) = happyGoto action_14
action_12 _ = happyFail

action_13 _ = happyReduce_14

action_14 (13) = happyShift action_27
action_14 (14) = happyShift action_28
action_14 _ = happyFail

action_15 (12) = happyShift action_26
action_15 _ = happyFail

action_16 (10) = happyShift action_25
action_16 (7) = happyGoto action_24
action_16 _ = happyFail

action_17 _ = happyReduce_7

action_18 (13) = happyShift action_23
action_18 _ = happyFail

action_19 (9) = happyShift action_6
action_19 (10) = happyShift action_7
action_19 (12) = happyShift action_8
action_19 (16) = happyShift action_9
action_19 (20) = happyShift action_12
action_19 (6) = happyGoto action_22
action_19 _ = happyFail

action_20 (15) = happyShift action_21
action_20 _ = happyFail

action_21 (9) = happyShift action_6
action_21 (10) = happyShift action_7
action_21 (12) = happyShift action_8
action_21 (16) = happyShift action_9
action_21 (20) = happyShift action_12
action_21 (6) = happyGoto action_33
action_21 _ = happyFail

action_22 _ = happyReduce_6

action_23 _ = happyReduce_8

action_24 (13) = happyShift action_31
action_24 (14) = happyShift action_32
action_24 _ = happyFail

action_25 _ = happyReduce_12

action_26 (9) = happyShift action_30
action_26 _ = happyFail

action_27 _ = happyReduce_9

action_28 (9) = happyShift action_6
action_28 (10) = happyShift action_7
action_28 (12) = happyShift action_8
action_28 (16) = happyShift action_9
action_28 (20) = happyShift action_12
action_28 (6) = happyGoto action_29
action_28 _ = happyFail

action_29 _ = happyReduce_15

action_30 (14) = happyShift action_36
action_30 _ = happyFail

action_31 (15) = happyShift action_35
action_31 _ = happyFail

action_32 (10) = happyShift action_34
action_32 _ = happyFail

action_33 _ = happyReduce_1

action_34 _ = happyReduce_13

action_35 (9) = happyShift action_6
action_35 (10) = happyShift action_7
action_35 (12) = happyShift action_8
action_35 (16) = happyShift action_9
action_35 (20) = happyShift action_12
action_35 (6) = happyGoto action_38
action_35 _ = happyFail

action_36 (9) = happyShift action_37
action_36 _ = happyFail

action_37 (13) = happyShift action_39
action_37 _ = happyFail

action_38 _ = happyReduce_2

action_39 (15) = happyShift action_40
action_39 _ = happyFail

action_40 (9) = happyShift action_6
action_40 (10) = happyShift action_7
action_40 (12) = happyShift action_8
action_40 (16) = happyShift action_9
action_40 (20) = happyShift action_12
action_40 (6) = happyGoto action_41
action_40 _ = happyFail

action_41 _ = happyReduce_3

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Asgn happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TFIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (UDF happy_var_2 (reverse happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 9 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNumber happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNumber happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (UDO happy_var_2 (truncate happy_var_4) (if happy_var_6 == 0 then L else R) happy_var_9
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal (TOp happy_var_2))
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (OpCall happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (UMinus happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Par happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TFIdent happy_var_1))
	 =  HappyAbsSyn6
		 (FunCall happy_var_1 (reverse happy_var_2)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyTerminal (TNumber happy_var_1))
	 =  HappyAbsSyn6
		 (Number happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn6
		 (Id happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyTerminal (TIdent happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TNumber happy_dollar_dollar -> cont 9;
	TIdent happy_dollar_dollar -> cont 10;
	TOp happy_dollar_dollar -> cont 11;
	TLPar -> cont 12;
	TRPar -> cont 13;
	TComma -> cont 14;
	TEqual -> cont 15;
	TMinus -> cont 16;
	TLet -> cont 17;
	TFun -> cont 18;
	TEnd -> cont 19;
	TFIdent happy_dollar_dollar -> cont 20;
	_ -> happyError' (tk:tks)
	}

happyError_ 21 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/home/dan/.stack/programs/x86_64-linux/ghc-nopie-8.0.2/lib/ghc-8.0.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc1258_0/ghc_2.h" #-}




















































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
