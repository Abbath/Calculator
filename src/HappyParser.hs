{-# OPTIONS_GHC -w #-}
module Calculator.HappyParser where
import Calculator.AlexLexer
import Calculator.Types
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16

action_0 (17) = happyShift action_14
action_0 (18) = happyShift action_15
action_0 (19) = happyShift action_16
action_0 (20) = happyShift action_17
action_0 (24) = happyShift action_18
action_0 (25) = happyShift action_3
action_0 (26) = happyShift action_19
action_0 (27) = happyShift action_20
action_0 (28) = happyShift action_21
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 (14) = happyGoto action_11
action_0 (15) = happyGoto action_12
action_0 (16) = happyGoto action_13
action_0 _ = happyFail

action_1 (25) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (18) = happyShift action_28
action_3 _ = happyFail

action_4 (29) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_2

action_6 _ = happyReduce_3

action_7 _ = happyReduce_4

action_8 _ = happyReduce_8

action_9 _ = happyReduce_9

action_10 _ = happyReduce_11

action_11 _ = happyReduce_12

action_12 _ = happyReduce_13

action_13 _ = happyReduce_10

action_14 _ = happyReduce_21

action_15 _ = happyReduce_22

action_16 (17) = happyShift action_14
action_16 (18) = happyShift action_15
action_16 (19) = happyShift action_16
action_16 (20) = happyShift action_17
action_16 (24) = happyShift action_18
action_16 (28) = happyShift action_21
action_16 (8) = happyGoto action_27
action_16 (10) = happyGoto action_8
action_16 (11) = happyGoto action_9
action_16 (12) = happyGoto action_10
action_16 (14) = happyGoto action_11
action_16 (15) = happyGoto action_12
action_16 (16) = happyGoto action_13
action_16 _ = happyFail

action_17 (17) = happyShift action_14
action_17 (18) = happyShift action_15
action_17 (19) = happyShift action_16
action_17 (20) = happyShift action_17
action_17 (24) = happyShift action_18
action_17 (28) = happyShift action_21
action_17 (8) = happyGoto action_26
action_17 (10) = happyGoto action_8
action_17 (11) = happyGoto action_9
action_17 (12) = happyGoto action_10
action_17 (14) = happyGoto action_11
action_17 (15) = happyGoto action_12
action_17 (16) = happyGoto action_13
action_17 _ = happyFail

action_18 (17) = happyShift action_14
action_18 (18) = happyShift action_15
action_18 (19) = happyShift action_16
action_18 (20) = happyShift action_17
action_18 (24) = happyShift action_18
action_18 (28) = happyShift action_21
action_18 (8) = happyGoto action_25
action_18 (10) = happyGoto action_8
action_18 (11) = happyGoto action_9
action_18 (12) = happyGoto action_10
action_18 (14) = happyGoto action_11
action_18 (15) = happyGoto action_12
action_18 (16) = happyGoto action_13
action_18 _ = happyFail

action_19 (18) = happyShift action_24
action_19 _ = happyFail

action_20 (18) = happyShift action_23
action_20 _ = happyFail

action_21 (20) = happyShift action_22
action_21 _ = happyFail

action_22 (17) = happyShift action_14
action_22 (18) = happyShift action_15
action_22 (19) = happyShift action_16
action_22 (20) = happyShift action_17
action_22 (24) = happyShift action_18
action_22 (28) = happyShift action_21
action_22 (8) = happyGoto action_34
action_22 (10) = happyGoto action_8
action_22 (11) = happyGoto action_9
action_22 (12) = happyGoto action_10
action_22 (13) = happyGoto action_35
action_22 (14) = happyGoto action_11
action_22 (15) = happyGoto action_12
action_22 (16) = happyGoto action_13
action_22 _ = happyFail

action_23 (20) = happyShift action_33
action_23 _ = happyFail

action_24 (20) = happyShift action_32
action_24 _ = happyFail

action_25 _ = happyReduce_16

action_26 (21) = happyShift action_31
action_26 _ = happyFail

action_27 (17) = happyShift action_14
action_27 (18) = happyShift action_15
action_27 (19) = happyShift action_16
action_27 (20) = happyShift action_17
action_27 (24) = happyShift action_18
action_27 (28) = happyShift action_21
action_27 (8) = happyGoto action_30
action_27 (10) = happyGoto action_8
action_27 (11) = happyGoto action_9
action_27 (12) = happyGoto action_10
action_27 (14) = happyGoto action_11
action_27 (15) = happyGoto action_12
action_27 (16) = happyGoto action_13
action_27 _ = happyFail

action_28 (23) = happyShift action_29
action_28 _ = happyFail

action_29 (17) = happyShift action_14
action_29 (18) = happyShift action_15
action_29 (19) = happyShift action_16
action_29 (20) = happyShift action_17
action_29 (24) = happyShift action_18
action_29 (28) = happyShift action_21
action_29 (8) = happyGoto action_41
action_29 (10) = happyGoto action_8
action_29 (11) = happyGoto action_9
action_29 (12) = happyGoto action_10
action_29 (14) = happyGoto action_11
action_29 (15) = happyGoto action_12
action_29 (16) = happyGoto action_13
action_29 _ = happyFail

action_30 _ = happyReduce_17

action_31 _ = happyReduce_23

action_32 (18) = happyShift action_15
action_32 (9) = happyGoto action_39
action_32 (15) = happyGoto action_40
action_32 _ = happyFail

action_33 (17) = happyShift action_38
action_33 _ = happyFail

action_34 _ = happyReduce_19

action_35 (21) = happyShift action_36
action_35 (22) = happyShift action_37
action_35 _ = happyFail

action_36 _ = happyReduce_18

action_37 (17) = happyShift action_14
action_37 (18) = happyShift action_15
action_37 (19) = happyShift action_16
action_37 (20) = happyShift action_17
action_37 (24) = happyShift action_18
action_37 (28) = happyShift action_21
action_37 (8) = happyGoto action_45
action_37 (10) = happyGoto action_8
action_37 (11) = happyGoto action_9
action_37 (12) = happyGoto action_10
action_37 (14) = happyGoto action_11
action_37 (15) = happyGoto action_12
action_37 (16) = happyGoto action_13
action_37 _ = happyFail

action_38 (22) = happyShift action_44
action_38 _ = happyFail

action_39 (21) = happyShift action_42
action_39 (22) = happyShift action_43
action_39 _ = happyFail

action_40 _ = happyReduce_14

action_41 _ = happyReduce_5

action_42 (23) = happyShift action_48
action_42 _ = happyFail

action_43 (18) = happyShift action_15
action_43 (15) = happyGoto action_47
action_43 _ = happyFail

action_44 (17) = happyShift action_46
action_44 _ = happyFail

action_45 _ = happyReduce_20

action_46 (21) = happyShift action_50
action_46 _ = happyFail

action_47 _ = happyReduce_15

action_48 (17) = happyShift action_14
action_48 (18) = happyShift action_15
action_48 (19) = happyShift action_16
action_48 (20) = happyShift action_17
action_48 (24) = happyShift action_18
action_48 (28) = happyShift action_21
action_48 (8) = happyGoto action_49
action_48 (10) = happyGoto action_8
action_48 (11) = happyGoto action_9
action_48 (12) = happyGoto action_10
action_48 (14) = happyGoto action_11
action_48 (15) = happyGoto action_12
action_48 (16) = happyGoto action_13
action_48 _ = happyFail

action_49 _ = happyReduce_6

action_50 (23) = happyShift action_51
action_50 _ = happyFail

action_51 (17) = happyShift action_14
action_51 (18) = happyShift action_15
action_51 (19) = happyShift action_16
action_51 (20) = happyShift action_17
action_51 (24) = happyShift action_18
action_51 (28) = happyShift action_21
action_51 (8) = happyGoto action_52
action_51 (10) = happyGoto action_8
action_51 (11) = happyGoto action_9
action_51 (12) = happyGoto action_10
action_51 (14) = happyGoto action_11
action_51 (15) = happyGoto action_12
action_51 (16) = happyGoto action_13
action_51 _ = happyFail

action_52 _ = happyReduce_7

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Asgn happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (UDF happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (UDO happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (Expr happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Asgn happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 7 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (UDF happy_var_2 (reverse happy_var_4) happy_var_7
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 9 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn8  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNumber happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNumber happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (UDO happy_var_2 (truncate happy_var_4) (if happy_var_6 == 0 then L else R) happy_var_9
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (UMinus happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (OpCall happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn8
		 (Par happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (FunCall happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (Number happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (Id happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (UMinus happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TOp happy_var_1))
	 =  HappyAbsSyn11
		 (OpCall happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TFIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (FunCall happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyTerminal (TNumber happy_var_1))
	 =  HappyAbsSyn14
		 (Number happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyTerminal (TIdent happy_var_1))
	 =  HappyAbsSyn15
		 (Id happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Par happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TNumber happy_dollar_dollar -> cont 17;
	TIdent happy_dollar_dollar -> cont 18;
	TOp happy_dollar_dollar -> cont 19;
	TLPar -> cont 20;
	TRPar -> cont 21;
	TComma -> cont 22;
	TEqual -> cont 23;
	TMinus -> cont 24;
	TLet -> cont 25;
	TFun -> cont 26;
	TEnd -> cont 27;
	TFIdent happy_dollar_dollar -> cont 28;
	_ -> happyError' (tk:tks)
	}

happyError_ 29 tk tks = happyError' tks
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
