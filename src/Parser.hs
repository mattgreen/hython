{-# OPTIONS_GHC -w #-}
module Parser(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24
	= HappyTerminal (L.Token)
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
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24

action_0 (25) = happyShift action_19
action_0 (26) = happyShift action_20
action_0 (27) = happyShift action_21
action_0 (36) = happyShift action_22
action_0 (37) = happyShift action_23
action_0 (38) = happyShift action_24
action_0 (39) = happyShift action_25
action_0 (42) = happyShift action_26
action_0 (43) = happyShift action_27
action_0 (44) = happyShift action_28
action_0 (45) = happyShift action_29
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 (14) = happyGoto action_11
action_0 (15) = happyGoto action_12
action_0 (16) = happyGoto action_13
action_0 (18) = happyGoto action_14
action_0 (19) = happyGoto action_15
action_0 (20) = happyGoto action_16
action_0 (21) = happyGoto action_17
action_0 (22) = happyGoto action_18
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (46) = happyAccept
action_2 _ = happyFail

action_3 (27) = happyShift action_41
action_3 _ = happyReduce_2

action_4 _ = happyReduce_3

action_5 _ = happyReduce_9

action_6 _ = happyReduce_8

action_7 _ = happyReduce_10

action_8 _ = happyReduce_11

action_9 _ = happyReduce_12

action_10 _ = happyReduce_13

action_11 _ = happyReduce_14

action_12 _ = happyReduce_15

action_13 _ = happyReduce_16

action_14 _ = happyReduce_17

action_15 _ = happyReduce_33

action_16 (28) = happyShift action_39
action_16 (29) = happyShift action_40
action_16 _ = happyReduce_30

action_17 _ = happyReduce_34

action_18 _ = happyReduce_35

action_19 (33) = happyShift action_38
action_19 _ = happyReduce_31

action_20 _ = happyReduce_32

action_21 (25) = happyShift action_19
action_21 (26) = happyShift action_20
action_21 (27) = happyShift action_21
action_21 (36) = happyShift action_22
action_21 (37) = happyShift action_23
action_21 (38) = happyShift action_24
action_21 (39) = happyShift action_25
action_21 (42) = happyShift action_26
action_21 (43) = happyShift action_27
action_21 (44) = happyShift action_28
action_21 (45) = happyShift action_29
action_21 (5) = happyGoto action_37
action_21 (6) = happyGoto action_4
action_21 (7) = happyGoto action_5
action_21 (8) = happyGoto action_6
action_21 (9) = happyGoto action_7
action_21 (10) = happyGoto action_8
action_21 (11) = happyGoto action_9
action_21 (12) = happyGoto action_10
action_21 (14) = happyGoto action_11
action_21 (15) = happyGoto action_12
action_21 (16) = happyGoto action_13
action_21 (18) = happyGoto action_14
action_21 (19) = happyGoto action_15
action_21 (20) = happyGoto action_16
action_21 (21) = happyGoto action_17
action_21 (22) = happyGoto action_18
action_21 _ = happyReduce_7

action_22 (25) = happyShift action_31
action_22 (26) = happyShift action_20
action_22 (17) = happyGoto action_36
action_22 (18) = happyGoto action_33
action_22 (19) = happyGoto action_15
action_22 (20) = happyGoto action_16
action_22 (21) = happyGoto action_17
action_22 (22) = happyGoto action_18
action_22 _ = happyFail

action_23 _ = happyReduce_20

action_24 _ = happyReduce_21

action_25 (25) = happyShift action_35
action_25 _ = happyFail

action_26 (25) = happyShift action_31
action_26 (26) = happyShift action_20
action_26 (18) = happyGoto action_34
action_26 (19) = happyGoto action_15
action_26 (20) = happyGoto action_16
action_26 (21) = happyGoto action_17
action_26 (22) = happyGoto action_18
action_26 _ = happyFail

action_27 _ = happyReduce_25

action_28 (25) = happyShift action_31
action_28 (26) = happyShift action_20
action_28 (17) = happyGoto action_32
action_28 (18) = happyGoto action_33
action_28 (19) = happyGoto action_15
action_28 (20) = happyGoto action_16
action_28 (21) = happyGoto action_17
action_28 (22) = happyGoto action_18
action_28 _ = happyReduce_26

action_29 (25) = happyShift action_31
action_29 (26) = happyShift action_20
action_29 (18) = happyGoto action_30
action_29 (19) = happyGoto action_15
action_29 (20) = happyGoto action_16
action_29 (21) = happyGoto action_17
action_29 (22) = happyGoto action_18
action_29 _ = happyFail

action_30 (31) = happyShift action_50
action_30 _ = happyFail

action_31 _ = happyReduce_31

action_32 _ = happyReduce_27

action_33 _ = happyReduce_29

action_34 (31) = happyShift action_49
action_34 _ = happyFail

action_35 (29) = happyShift action_48
action_35 _ = happyFail

action_36 _ = happyReduce_18

action_37 (27) = happyShift action_41
action_37 _ = happyReduce_6

action_38 (25) = happyShift action_31
action_38 (26) = happyShift action_20
action_38 (17) = happyGoto action_47
action_38 (18) = happyGoto action_33
action_38 (19) = happyGoto action_15
action_38 (20) = happyGoto action_16
action_38 (21) = happyGoto action_17
action_38 (22) = happyGoto action_18
action_38 _ = happyFail

action_39 (25) = happyShift action_46
action_39 _ = happyFail

action_40 (25) = happyShift action_31
action_40 (26) = happyShift action_20
action_40 (19) = happyGoto action_15
action_40 (20) = happyGoto action_43
action_40 (21) = happyGoto action_17
action_40 (22) = happyGoto action_18
action_40 (23) = happyGoto action_44
action_40 (24) = happyGoto action_45
action_40 _ = happyReduce_38

action_41 (25) = happyShift action_19
action_41 (26) = happyShift action_20
action_41 (36) = happyShift action_22
action_41 (37) = happyShift action_23
action_41 (38) = happyShift action_24
action_41 (39) = happyShift action_25
action_41 (42) = happyShift action_26
action_41 (43) = happyShift action_27
action_41 (44) = happyShift action_28
action_41 (45) = happyShift action_29
action_41 (6) = happyGoto action_42
action_41 (7) = happyGoto action_5
action_41 (8) = happyGoto action_6
action_41 (9) = happyGoto action_7
action_41 (10) = happyGoto action_8
action_41 (11) = happyGoto action_9
action_41 (12) = happyGoto action_10
action_41 (14) = happyGoto action_11
action_41 (15) = happyGoto action_12
action_41 (16) = happyGoto action_13
action_41 (18) = happyGoto action_14
action_41 (19) = happyGoto action_15
action_41 (20) = happyGoto action_16
action_41 (21) = happyGoto action_17
action_41 (22) = happyGoto action_18
action_41 _ = happyReduce_5

action_42 _ = happyReduce_4

action_43 (28) = happyShift action_39
action_43 (29) = happyShift action_40
action_43 _ = happyReduce_41

action_44 (30) = happyShift action_56
action_44 _ = happyFail

action_45 (32) = happyShift action_55
action_45 _ = happyReduce_39

action_46 _ = happyReduce_36

action_47 _ = happyReduce_19

action_48 (30) = happyShift action_54
action_48 _ = happyFail

action_49 (27) = happyShift action_52
action_49 (13) = happyGoto action_53
action_49 _ = happyFail

action_50 (27) = happyShift action_52
action_50 (13) = happyGoto action_51
action_50 _ = happyFail

action_51 _ = happyReduce_28

action_52 (34) = happyShift action_59
action_52 _ = happyFail

action_53 _ = happyReduce_23

action_54 (31) = happyShift action_58
action_54 _ = happyFail

action_55 (25) = happyShift action_31
action_55 (26) = happyShift action_20
action_55 (19) = happyGoto action_15
action_55 (20) = happyGoto action_43
action_55 (21) = happyGoto action_17
action_55 (22) = happyGoto action_18
action_55 (23) = happyGoto action_57
action_55 (24) = happyGoto action_45
action_55 _ = happyReduce_38

action_56 _ = happyReduce_37

action_57 _ = happyReduce_40

action_58 (27) = happyShift action_52
action_58 (13) = happyGoto action_61
action_58 _ = happyFail

action_59 (25) = happyShift action_19
action_59 (26) = happyShift action_20
action_59 (27) = happyShift action_21
action_59 (36) = happyShift action_22
action_59 (37) = happyShift action_23
action_59 (38) = happyShift action_24
action_59 (39) = happyShift action_25
action_59 (42) = happyShift action_26
action_59 (43) = happyShift action_27
action_59 (44) = happyShift action_28
action_59 (45) = happyShift action_29
action_59 (5) = happyGoto action_60
action_59 (6) = happyGoto action_4
action_59 (7) = happyGoto action_5
action_59 (8) = happyGoto action_6
action_59 (9) = happyGoto action_7
action_59 (10) = happyGoto action_8
action_59 (11) = happyGoto action_9
action_59 (12) = happyGoto action_10
action_59 (14) = happyGoto action_11
action_59 (15) = happyGoto action_12
action_59 (16) = happyGoto action_13
action_59 (18) = happyGoto action_14
action_59 (19) = happyGoto action_15
action_59 (20) = happyGoto action_16
action_59 (21) = happyGoto action_17
action_59 (22) = happyGoto action_18
action_59 _ = happyFail

action_60 (27) = happyShift action_41
action_60 _ = happyReduce_24

action_61 _ = happyReduce_22

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 ([]
	)

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn6
		 (Expression happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  7 happyReduction_18
happyReduction_18 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Assert happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn8
		 (Assignment (Variable happy_var_1) happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn9
		 (Break
	)

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn10
		 (Continue
	)

happyReduce_22 = happyReduce 6 11 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Def happy_var_2 [] happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (If [(IfClause happy_var_2 happy_var_4)] []
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn13
		 (happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn14
		 (Pass
	)

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn15
		 (Return $ Constant None
	)

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Return happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 16 happyReduction_28
happyReduction_28 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn19
		 (Variable happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyTerminal (L.Literal happy_var_1))
	 =  HappyAbsSyn19
		 (Constant happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  20 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  20 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 (HappyTerminal (L.Identifier happy_var_3))
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 (Attribute happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 22 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_0  23 happyReduction_38
happyReduction_38  =  HappyAbsSyn23
		 ([]
	)

happyReduce_39 = happySpecReduce_1  23 happyReduction_39
happyReduction_39 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  23 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyNewToken action sts stk
	= L.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.EOF -> action 46 46 tk (HappyState action) sts stk;
	L.Identifier happy_dollar_dollar -> cont 25;
	L.Literal happy_dollar_dollar -> cont 26;
	L.Newline -> cont 27;
	L.Punctuation "." -> cont 28;
	L.Punctuation "(" -> cont 29;
	L.Punctuation ")" -> cont 30;
	L.Punctuation ":" -> cont 31;
	L.Punctuation "," -> cont 32;
	L.Punctuation "=" -> cont 33;
	L.Indent -> cont 34;
	L.Dedent -> cont 35;
	L.Keyword "assert" -> cont 36;
	L.Keyword "break" -> cont 37;
	L.Keyword "continue" -> cont 38;
	L.Keyword "def" -> cont 39;
	L.Keyword "elif" -> cont 40;
	L.Keyword "else" -> cont 41;
	L.Keyword "if" -> cont 42;
	L.Keyword "pass" -> cont 43;
	L.Keyword "return" -> cont 44;
	L.Keyword "while" -> cont 45;
	_ -> happyError' tk
	})

happyError_ 46 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => L.P a -> (a -> L.P b) -> L.P b
happyThen = (>>=)
happyReturn :: () => a -> L.P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> L.P a
happyReturn1 = happyReturn
happyError' :: () => (L.Token) -> L.P a
happyError' tk = parseError tk

parseTokens = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parse code = L.evalP parseTokens code

parseError :: L.Token -> a
parseError t = error $ "Parse error: " ++ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
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
