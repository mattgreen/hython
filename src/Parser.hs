{-# OPTIONS_GHC -w #-}
module Parser(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
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
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

action_0 (31) = happyShift action_20
action_0 (32) = happyShift action_21
action_0 (33) = happyShift action_22
action_0 (42) = happyShift action_23
action_0 (43) = happyShift action_24
action_0 (44) = happyShift action_25
action_0 (45) = happyShift action_26
action_0 (46) = happyShift action_27
action_0 (49) = happyShift action_28
action_0 (50) = happyShift action_29
action_0 (51) = happyShift action_30
action_0 (52) = happyShift action_31
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 (13) = happyGoto action_11
action_0 (15) = happyGoto action_12
action_0 (16) = happyGoto action_13
action_0 (17) = happyGoto action_14
action_0 (19) = happyGoto action_15
action_0 (24) = happyGoto action_16
action_0 (25) = happyGoto action_17
action_0 (26) = happyGoto action_18
action_0 (27) = happyGoto action_19
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (53) = happyAccept
action_2 _ = happyFail

action_3 (33) = happyShift action_44
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

action_15 _ = happyReduce_18

action_16 _ = happyReduce_43

action_17 (34) = happyShift action_42
action_17 (35) = happyShift action_43
action_17 _ = happyReduce_34

action_18 _ = happyReduce_44

action_19 _ = happyReduce_45

action_20 (39) = happyShift action_41
action_20 _ = happyReduce_41

action_21 _ = happyReduce_42

action_22 (31) = happyShift action_20
action_22 (32) = happyShift action_21
action_22 (33) = happyShift action_22
action_22 (42) = happyShift action_23
action_22 (43) = happyShift action_24
action_22 (44) = happyShift action_25
action_22 (45) = happyShift action_26
action_22 (46) = happyShift action_27
action_22 (49) = happyShift action_28
action_22 (50) = happyShift action_29
action_22 (51) = happyShift action_30
action_22 (52) = happyShift action_31
action_22 (5) = happyGoto action_40
action_22 (6) = happyGoto action_4
action_22 (7) = happyGoto action_5
action_22 (8) = happyGoto action_6
action_22 (9) = happyGoto action_7
action_22 (10) = happyGoto action_8
action_22 (11) = happyGoto action_9
action_22 (12) = happyGoto action_10
action_22 (13) = happyGoto action_11
action_22 (15) = happyGoto action_12
action_22 (16) = happyGoto action_13
action_22 (17) = happyGoto action_14
action_22 (19) = happyGoto action_15
action_22 (24) = happyGoto action_16
action_22 (25) = happyGoto action_17
action_22 (26) = happyGoto action_18
action_22 (27) = happyGoto action_19
action_22 _ = happyReduce_7

action_23 (31) = happyShift action_33
action_23 (32) = happyShift action_21
action_23 (18) = happyGoto action_39
action_23 (19) = happyGoto action_35
action_23 (24) = happyGoto action_16
action_23 (25) = happyGoto action_17
action_23 (26) = happyGoto action_18
action_23 (27) = happyGoto action_19
action_23 _ = happyFail

action_24 _ = happyReduce_21

action_25 (31) = happyShift action_38
action_25 _ = happyFail

action_26 _ = happyReduce_24

action_27 (31) = happyShift action_37
action_27 _ = happyFail

action_28 (31) = happyShift action_33
action_28 (32) = happyShift action_21
action_28 (19) = happyGoto action_36
action_28 (24) = happyGoto action_16
action_28 (25) = happyGoto action_17
action_28 (26) = happyGoto action_18
action_28 (27) = happyGoto action_19
action_28 _ = happyFail

action_29 _ = happyReduce_29

action_30 (31) = happyShift action_33
action_30 (32) = happyShift action_21
action_30 (18) = happyGoto action_34
action_30 (19) = happyGoto action_35
action_30 (24) = happyGoto action_16
action_30 (25) = happyGoto action_17
action_30 (26) = happyGoto action_18
action_30 (27) = happyGoto action_19
action_30 _ = happyReduce_30

action_31 (31) = happyShift action_33
action_31 (32) = happyShift action_21
action_31 (19) = happyGoto action_32
action_31 (24) = happyGoto action_16
action_31 (25) = happyGoto action_17
action_31 (26) = happyGoto action_18
action_31 (27) = happyGoto action_19
action_31 _ = happyFail

action_32 (37) = happyShift action_56
action_32 _ = happyFail

action_33 _ = happyReduce_41

action_34 _ = happyReduce_31

action_35 _ = happyReduce_33

action_36 (37) = happyShift action_55
action_36 _ = happyFail

action_37 (35) = happyShift action_54
action_37 _ = happyFail

action_38 (35) = happyShift action_52
action_38 (37) = happyShift action_53
action_38 _ = happyFail

action_39 _ = happyReduce_19

action_40 (33) = happyShift action_44
action_40 _ = happyReduce_6

action_41 (31) = happyShift action_33
action_41 (32) = happyShift action_21
action_41 (18) = happyGoto action_51
action_41 (19) = happyGoto action_35
action_41 (24) = happyGoto action_16
action_41 (25) = happyGoto action_17
action_41 (26) = happyGoto action_18
action_41 (27) = happyGoto action_19
action_41 _ = happyFail

action_42 (31) = happyShift action_50
action_42 _ = happyFail

action_43 (31) = happyShift action_33
action_43 (32) = happyShift action_21
action_43 (24) = happyGoto action_16
action_43 (25) = happyGoto action_46
action_43 (26) = happyGoto action_18
action_43 (27) = happyGoto action_19
action_43 (28) = happyGoto action_47
action_43 (29) = happyGoto action_48
action_43 (30) = happyGoto action_49
action_43 _ = happyReduce_48

action_44 (31) = happyShift action_20
action_44 (32) = happyShift action_21
action_44 (42) = happyShift action_23
action_44 (43) = happyShift action_24
action_44 (44) = happyShift action_25
action_44 (45) = happyShift action_26
action_44 (46) = happyShift action_27
action_44 (49) = happyShift action_28
action_44 (50) = happyShift action_29
action_44 (51) = happyShift action_30
action_44 (52) = happyShift action_31
action_44 (6) = happyGoto action_45
action_44 (7) = happyGoto action_5
action_44 (8) = happyGoto action_6
action_44 (9) = happyGoto action_7
action_44 (10) = happyGoto action_8
action_44 (11) = happyGoto action_9
action_44 (12) = happyGoto action_10
action_44 (13) = happyGoto action_11
action_44 (15) = happyGoto action_12
action_44 (16) = happyGoto action_13
action_44 (17) = happyGoto action_14
action_44 (19) = happyGoto action_15
action_44 (24) = happyGoto action_16
action_44 (25) = happyGoto action_17
action_44 (26) = happyGoto action_18
action_44 (27) = happyGoto action_19
action_44 _ = happyReduce_5

action_45 _ = happyReduce_4

action_46 (34) = happyShift action_42
action_46 (35) = happyShift action_43
action_46 _ = happyReduce_52

action_47 (36) = happyShift action_67
action_47 _ = happyFail

action_48 _ = happyReduce_49

action_49 (38) = happyShift action_66
action_49 _ = happyReduce_50

action_50 _ = happyReduce_46

action_51 _ = happyReduce_20

action_52 (31) = happyShift action_63
action_52 (21) = happyGoto action_65
action_52 (22) = happyGoto action_61
action_52 (23) = happyGoto action_62
action_52 _ = happyReduce_36

action_53 (33) = happyShift action_58
action_53 (20) = happyGoto action_64
action_53 _ = happyFail

action_54 (31) = happyShift action_63
action_54 (21) = happyGoto action_60
action_54 (22) = happyGoto action_61
action_54 (23) = happyGoto action_62
action_54 _ = happyReduce_36

action_55 (33) = happyShift action_58
action_55 (20) = happyGoto action_59
action_55 _ = happyFail

action_56 (33) = happyShift action_58
action_56 (20) = happyGoto action_57
action_56 _ = happyFail

action_57 _ = happyReduce_32

action_58 (40) = happyShift action_74
action_58 _ = happyFail

action_59 (48) = happyShift action_73
action_59 (14) = happyGoto action_72
action_59 _ = happyReduce_27

action_60 (36) = happyShift action_71
action_60 _ = happyFail

action_61 _ = happyReduce_37

action_62 (38) = happyShift action_70
action_62 _ = happyReduce_38

action_63 _ = happyReduce_40

action_64 _ = happyReduce_22

action_65 (36) = happyShift action_69
action_65 _ = happyFail

action_66 (31) = happyShift action_33
action_66 (32) = happyShift action_21
action_66 (24) = happyGoto action_16
action_66 (25) = happyGoto action_46
action_66 (26) = happyGoto action_18
action_66 (27) = happyGoto action_19
action_66 (29) = happyGoto action_68
action_66 (30) = happyGoto action_49
action_66 _ = happyFail

action_67 _ = happyReduce_47

action_68 _ = happyReduce_51

action_69 (33) = happyShift action_58
action_69 (20) = happyGoto action_79
action_69 _ = happyFail

action_70 (31) = happyShift action_63
action_70 (22) = happyGoto action_78
action_70 (23) = happyGoto action_62
action_70 _ = happyFail

action_71 (37) = happyShift action_77
action_71 _ = happyFail

action_72 _ = happyReduce_26

action_73 (37) = happyShift action_76
action_73 _ = happyFail

action_74 (31) = happyShift action_20
action_74 (32) = happyShift action_21
action_74 (33) = happyShift action_22
action_74 (42) = happyShift action_23
action_74 (43) = happyShift action_24
action_74 (44) = happyShift action_25
action_74 (45) = happyShift action_26
action_74 (46) = happyShift action_27
action_74 (49) = happyShift action_28
action_74 (50) = happyShift action_29
action_74 (51) = happyShift action_30
action_74 (52) = happyShift action_31
action_74 (5) = happyGoto action_75
action_74 (6) = happyGoto action_4
action_74 (7) = happyGoto action_5
action_74 (8) = happyGoto action_6
action_74 (9) = happyGoto action_7
action_74 (10) = happyGoto action_8
action_74 (11) = happyGoto action_9
action_74 (12) = happyGoto action_10
action_74 (13) = happyGoto action_11
action_74 (15) = happyGoto action_12
action_74 (16) = happyGoto action_13
action_74 (17) = happyGoto action_14
action_74 (19) = happyGoto action_15
action_74 (24) = happyGoto action_16
action_74 (25) = happyGoto action_17
action_74 (26) = happyGoto action_18
action_74 (27) = happyGoto action_19
action_74 _ = happyFail

action_75 (33) = happyShift action_44
action_75 _ = happyReduce_35

action_76 (33) = happyShift action_58
action_76 (20) = happyGoto action_81
action_76 _ = happyFail

action_77 (33) = happyShift action_58
action_77 (20) = happyGoto action_80
action_77 _ = happyFail

action_78 _ = happyReduce_39

action_79 _ = happyReduce_23

action_80 _ = happyReduce_25

action_81 _ = happyReduce_28

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
happyReduction_14 (HappyAbsSyn13  happy_var_1)
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
happyReduction_17 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn6
		 (Expression happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Assert happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_3)
	_
	(HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn8
		 (Assignment (Variable happy_var_1) happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn9
		 (Break
	)

happyReduce_22 = happyReduce 4 10 happyReduction_22
happyReduction_22 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ClassDef happy_var_2 [] happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 10 happyReduction_23
happyReduction_23 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ClassDef happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn11
		 (Continue
	)

happyReduce_25 = happyReduce 7 12 happyReduction_25
happyReduction_25 ((HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Def happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 13 happyReduction_26
happyReduction_26 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (If [(IfClause happy_var_2 happy_var_4)] happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_0  14 happyReduction_27
happyReduction_27  =  HappyAbsSyn14
		 ([]
	)

happyReduce_28 = happySpecReduce_3  14 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_3)
	_
	_
	 =  HappyAbsSyn14
		 (happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn15
		 (Pass
	)

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn16
		 (Return $ Constant None
	)

happyReduce_31 = happySpecReduce_2  16 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Return happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 17 happyReduction_32
happyReduction_32 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  20 happyReduction_35
happyReduction_35 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn20
		 (happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  21 happyReduction_36
happyReduction_36  =  HappyAbsSyn21
		 ([]
	)

happyReduce_37 = happySpecReduce_1  21 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1:happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn24
		 (Variable happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyTerminal (L.Literal happy_var_1))
	 =  HappyAbsSyn24
		 (Constant happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  26 happyReduction_46
happyReduction_46 (HappyTerminal (L.Identifier happy_var_3))
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Attribute happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 4 27 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_0  28 happyReduction_48
happyReduction_48  =  HappyAbsSyn28
		 ([]
	)

happyReduce_49 = happySpecReduce_1  28 happyReduction_49
happyReduction_49 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  29 happyReduction_50
happyReduction_50 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  29 happyReduction_51
happyReduction_51 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1:happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  30 happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyNewToken action sts stk
	= L.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.EOF -> action 53 53 tk (HappyState action) sts stk;
	L.Identifier happy_dollar_dollar -> cont 31;
	L.Literal happy_dollar_dollar -> cont 32;
	L.Newline -> cont 33;
	L.Punctuation "." -> cont 34;
	L.Punctuation "(" -> cont 35;
	L.Punctuation ")" -> cont 36;
	L.Punctuation ":" -> cont 37;
	L.Punctuation "," -> cont 38;
	L.Punctuation "=" -> cont 39;
	L.Indent -> cont 40;
	L.Dedent -> cont 41;
	L.Keyword "assert" -> cont 42;
	L.Keyword "break" -> cont 43;
	L.Keyword "class" -> cont 44;
	L.Keyword "continue" -> cont 45;
	L.Keyword "def" -> cont 46;
	L.Keyword "elif" -> cont 47;
	L.Keyword "else" -> cont 48;
	L.Keyword "if" -> cont 49;
	L.Keyword "pass" -> cont 50;
	L.Keyword "return" -> cont 51;
	L.Keyword "while" -> cont 52;
	_ -> happyError' tk
	})

happyError_ 53 tk = happyError' tk
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
