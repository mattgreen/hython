{-# OPTIONS_GHC -w #-}
module Parser(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32
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
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32

action_0 (33) = happyShift action_20
action_0 (34) = happyShift action_21
action_0 (35) = happyShift action_22
action_0 (54) = happyShift action_23
action_0 (55) = happyShift action_24
action_0 (56) = happyShift action_25
action_0 (57) = happyShift action_26
action_0 (58) = happyShift action_27
action_0 (61) = happyShift action_28
action_0 (62) = happyShift action_29
action_0 (63) = happyShift action_30
action_0 (64) = happyShift action_31
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
action_0 (17) = happyGoto action_12
action_0 (18) = happyGoto action_13
action_0 (19) = happyGoto action_14
action_0 (21) = happyGoto action_15
action_0 (26) = happyGoto action_16
action_0 (27) = happyGoto action_17
action_0 (28) = happyGoto action_18
action_0 (29) = happyGoto action_19
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (65) = happyAccept
action_2 _ = happyFail

action_3 (35) = happyShift action_54
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

action_16 _ = happyReduce_57

action_17 (36) = happyShift action_42
action_17 (37) = happyShift action_43
action_17 (38) = happyShift action_44
action_17 (39) = happyShift action_45
action_17 (40) = happyShift action_46
action_17 (41) = happyShift action_47
action_17 (42) = happyShift action_48
action_17 (43) = happyShift action_49
action_17 (44) = happyShift action_50
action_17 (45) = happyShift action_51
action_17 (46) = happyShift action_52
action_17 (47) = happyShift action_53
action_17 _ = happyReduce_48

action_18 _ = happyReduce_58

action_19 _ = happyReduce_59

action_20 (51) = happyShift action_41
action_20 _ = happyReduce_55

action_21 _ = happyReduce_56

action_22 (33) = happyShift action_20
action_22 (34) = happyShift action_21
action_22 (35) = happyShift action_22
action_22 (54) = happyShift action_23
action_22 (55) = happyShift action_24
action_22 (56) = happyShift action_25
action_22 (57) = happyShift action_26
action_22 (58) = happyShift action_27
action_22 (61) = happyShift action_28
action_22 (62) = happyShift action_29
action_22 (63) = happyShift action_30
action_22 (64) = happyShift action_31
action_22 (5) = happyGoto action_40
action_22 (6) = happyGoto action_4
action_22 (7) = happyGoto action_5
action_22 (8) = happyGoto action_6
action_22 (9) = happyGoto action_7
action_22 (10) = happyGoto action_8
action_22 (11) = happyGoto action_9
action_22 (12) = happyGoto action_10
action_22 (13) = happyGoto action_11
action_22 (17) = happyGoto action_12
action_22 (18) = happyGoto action_13
action_22 (19) = happyGoto action_14
action_22 (21) = happyGoto action_15
action_22 (26) = happyGoto action_16
action_22 (27) = happyGoto action_17
action_22 (28) = happyGoto action_18
action_22 (29) = happyGoto action_19
action_22 _ = happyReduce_7

action_23 (33) = happyShift action_33
action_23 (34) = happyShift action_21
action_23 (20) = happyGoto action_39
action_23 (21) = happyGoto action_35
action_23 (26) = happyGoto action_16
action_23 (27) = happyGoto action_17
action_23 (28) = happyGoto action_18
action_23 (29) = happyGoto action_19
action_23 _ = happyFail

action_24 _ = happyReduce_21

action_25 (33) = happyShift action_38
action_25 _ = happyFail

action_26 _ = happyReduce_24

action_27 (33) = happyShift action_37
action_27 _ = happyFail

action_28 (33) = happyShift action_33
action_28 (34) = happyShift action_21
action_28 (21) = happyGoto action_36
action_28 (26) = happyGoto action_16
action_28 (27) = happyGoto action_17
action_28 (28) = happyGoto action_18
action_28 (29) = happyGoto action_19
action_28 _ = happyFail

action_29 _ = happyReduce_33

action_30 (33) = happyShift action_33
action_30 (34) = happyShift action_21
action_30 (20) = happyGoto action_34
action_30 (21) = happyGoto action_35
action_30 (26) = happyGoto action_16
action_30 (27) = happyGoto action_17
action_30 (28) = happyGoto action_18
action_30 (29) = happyGoto action_19
action_30 _ = happyReduce_34

action_31 (33) = happyShift action_33
action_31 (34) = happyShift action_21
action_31 (21) = happyGoto action_32
action_31 (26) = happyGoto action_16
action_31 (27) = happyGoto action_17
action_31 (28) = happyGoto action_18
action_31 (29) = happyGoto action_19
action_31 _ = happyFail

action_32 (49) = happyShift action_76
action_32 _ = happyFail

action_33 _ = happyReduce_55

action_34 _ = happyReduce_35

action_35 _ = happyReduce_37

action_36 (49) = happyShift action_75
action_36 _ = happyFail

action_37 (47) = happyShift action_74
action_37 _ = happyFail

action_38 (47) = happyShift action_72
action_38 (49) = happyShift action_73
action_38 _ = happyFail

action_39 _ = happyReduce_19

action_40 (35) = happyShift action_54
action_40 _ = happyReduce_6

action_41 (33) = happyShift action_33
action_41 (34) = happyShift action_21
action_41 (20) = happyGoto action_71
action_41 (21) = happyGoto action_35
action_41 (26) = happyGoto action_16
action_41 (27) = happyGoto action_17
action_41 (28) = happyGoto action_18
action_41 (29) = happyGoto action_19
action_41 _ = happyFail

action_42 (33) = happyShift action_33
action_42 (34) = happyShift action_21
action_42 (26) = happyGoto action_16
action_42 (27) = happyGoto action_70
action_42 (28) = happyGoto action_18
action_42 (29) = happyGoto action_19
action_42 _ = happyFail

action_43 (33) = happyShift action_33
action_43 (34) = happyShift action_21
action_43 (26) = happyGoto action_16
action_43 (27) = happyGoto action_69
action_43 (28) = happyGoto action_18
action_43 (29) = happyGoto action_19
action_43 _ = happyFail

action_44 (33) = happyShift action_33
action_44 (34) = happyShift action_21
action_44 (26) = happyGoto action_16
action_44 (27) = happyGoto action_68
action_44 (28) = happyGoto action_18
action_44 (29) = happyGoto action_19
action_44 _ = happyFail

action_45 (33) = happyShift action_33
action_45 (34) = happyShift action_21
action_45 (26) = happyGoto action_16
action_45 (27) = happyGoto action_67
action_45 (28) = happyGoto action_18
action_45 (29) = happyGoto action_19
action_45 _ = happyFail

action_46 (33) = happyShift action_33
action_46 (34) = happyShift action_21
action_46 (26) = happyGoto action_16
action_46 (27) = happyGoto action_66
action_46 (28) = happyGoto action_18
action_46 (29) = happyGoto action_19
action_46 _ = happyFail

action_47 (33) = happyShift action_33
action_47 (34) = happyShift action_21
action_47 (26) = happyGoto action_16
action_47 (27) = happyGoto action_65
action_47 (28) = happyGoto action_18
action_47 (29) = happyGoto action_19
action_47 _ = happyFail

action_48 (33) = happyShift action_33
action_48 (34) = happyShift action_21
action_48 (26) = happyGoto action_16
action_48 (27) = happyGoto action_64
action_48 (28) = happyGoto action_18
action_48 (29) = happyGoto action_19
action_48 _ = happyFail

action_49 (33) = happyShift action_33
action_49 (34) = happyShift action_21
action_49 (26) = happyGoto action_16
action_49 (27) = happyGoto action_63
action_49 (28) = happyGoto action_18
action_49 (29) = happyGoto action_19
action_49 _ = happyFail

action_50 (33) = happyShift action_33
action_50 (34) = happyShift action_21
action_50 (26) = happyGoto action_16
action_50 (27) = happyGoto action_62
action_50 (28) = happyGoto action_18
action_50 (29) = happyGoto action_19
action_50 _ = happyFail

action_51 (33) = happyShift action_33
action_51 (34) = happyShift action_21
action_51 (26) = happyGoto action_16
action_51 (27) = happyGoto action_61
action_51 (28) = happyGoto action_18
action_51 (29) = happyGoto action_19
action_51 _ = happyFail

action_52 (33) = happyShift action_60
action_52 _ = happyFail

action_53 (33) = happyShift action_33
action_53 (34) = happyShift action_21
action_53 (26) = happyGoto action_16
action_53 (27) = happyGoto action_56
action_53 (28) = happyGoto action_18
action_53 (29) = happyGoto action_19
action_53 (30) = happyGoto action_57
action_53 (31) = happyGoto action_58
action_53 (32) = happyGoto action_59
action_53 _ = happyReduce_62

action_54 (33) = happyShift action_20
action_54 (34) = happyShift action_21
action_54 (54) = happyShift action_23
action_54 (55) = happyShift action_24
action_54 (56) = happyShift action_25
action_54 (57) = happyShift action_26
action_54 (58) = happyShift action_27
action_54 (61) = happyShift action_28
action_54 (62) = happyShift action_29
action_54 (63) = happyShift action_30
action_54 (64) = happyShift action_31
action_54 (6) = happyGoto action_55
action_54 (7) = happyGoto action_5
action_54 (8) = happyGoto action_6
action_54 (9) = happyGoto action_7
action_54 (10) = happyGoto action_8
action_54 (11) = happyGoto action_9
action_54 (12) = happyGoto action_10
action_54 (13) = happyGoto action_11
action_54 (17) = happyGoto action_12
action_54 (18) = happyGoto action_13
action_54 (19) = happyGoto action_14
action_54 (21) = happyGoto action_15
action_54 (26) = happyGoto action_16
action_54 (27) = happyGoto action_17
action_54 (28) = happyGoto action_18
action_54 (29) = happyGoto action_19
action_54 _ = happyReduce_5

action_55 _ = happyReduce_4

action_56 (46) = happyShift action_52
action_56 (47) = happyShift action_53
action_56 _ = happyReduce_66

action_57 (48) = happyShift action_87
action_57 _ = happyFail

action_58 _ = happyReduce_63

action_59 (50) = happyShift action_86
action_59 _ = happyReduce_64

action_60 _ = happyReduce_60

action_61 (46) = happyShift action_52
action_61 (47) = happyShift action_53
action_61 _ = happyReduce_47

action_62 (46) = happyShift action_52
action_62 (47) = happyShift action_53
action_62 _ = happyReduce_46

action_63 (46) = happyShift action_52
action_63 (47) = happyShift action_53
action_63 _ = happyReduce_45

action_64 (46) = happyShift action_52
action_64 (47) = happyShift action_53
action_64 _ = happyReduce_44

action_65 (46) = happyShift action_52
action_65 (47) = happyShift action_53
action_65 _ = happyReduce_43

action_66 (46) = happyShift action_52
action_66 (47) = happyShift action_53
action_66 _ = happyReduce_42

action_67 (46) = happyShift action_52
action_67 (47) = happyShift action_53
action_67 _ = happyReduce_39

action_68 (46) = happyShift action_52
action_68 (47) = happyShift action_53
action_68 _ = happyReduce_38

action_69 (46) = happyShift action_52
action_69 (47) = happyShift action_53
action_69 _ = happyReduce_41

action_70 (46) = happyShift action_52
action_70 (47) = happyShift action_53
action_70 _ = happyReduce_40

action_71 _ = happyReduce_20

action_72 (33) = happyShift action_83
action_72 (23) = happyGoto action_85
action_72 (24) = happyGoto action_81
action_72 (25) = happyGoto action_82
action_72 _ = happyReduce_50

action_73 (35) = happyShift action_78
action_73 (22) = happyGoto action_84
action_73 _ = happyFail

action_74 (33) = happyShift action_83
action_74 (23) = happyGoto action_80
action_74 (24) = happyGoto action_81
action_74 (25) = happyGoto action_82
action_74 _ = happyReduce_50

action_75 (35) = happyShift action_78
action_75 (22) = happyGoto action_79
action_75 _ = happyFail

action_76 (35) = happyShift action_78
action_76 (22) = happyGoto action_77
action_76 _ = happyFail

action_77 _ = happyReduce_36

action_78 (52) = happyShift action_95
action_78 _ = happyFail

action_79 (59) = happyShift action_94
action_79 (14) = happyGoto action_92
action_79 (15) = happyGoto action_93
action_79 _ = happyReduce_27

action_80 (48) = happyShift action_91
action_80 _ = happyFail

action_81 _ = happyReduce_51

action_82 (50) = happyShift action_90
action_82 _ = happyReduce_52

action_83 _ = happyReduce_54

action_84 _ = happyReduce_22

action_85 (48) = happyShift action_89
action_85 _ = happyFail

action_86 (33) = happyShift action_33
action_86 (34) = happyShift action_21
action_86 (26) = happyGoto action_16
action_86 (27) = happyGoto action_56
action_86 (28) = happyGoto action_18
action_86 (29) = happyGoto action_19
action_86 (31) = happyGoto action_88
action_86 (32) = happyGoto action_59
action_86 _ = happyFail

action_87 _ = happyReduce_61

action_88 _ = happyReduce_65

action_89 (35) = happyShift action_78
action_89 (22) = happyGoto action_102
action_89 _ = happyFail

action_90 (33) = happyShift action_83
action_90 (24) = happyGoto action_101
action_90 (25) = happyGoto action_82
action_90 _ = happyFail

action_91 (49) = happyShift action_100
action_91 _ = happyFail

action_92 (60) = happyShift action_99
action_92 (16) = happyGoto action_98
action_92 _ = happyReduce_31

action_93 _ = happyReduce_28

action_94 (33) = happyShift action_33
action_94 (34) = happyShift action_21
action_94 (21) = happyGoto action_97
action_94 (26) = happyGoto action_16
action_94 (27) = happyGoto action_17
action_94 (28) = happyGoto action_18
action_94 (29) = happyGoto action_19
action_94 _ = happyFail

action_95 (33) = happyShift action_20
action_95 (34) = happyShift action_21
action_95 (35) = happyShift action_22
action_95 (54) = happyShift action_23
action_95 (55) = happyShift action_24
action_95 (56) = happyShift action_25
action_95 (57) = happyShift action_26
action_95 (58) = happyShift action_27
action_95 (61) = happyShift action_28
action_95 (62) = happyShift action_29
action_95 (63) = happyShift action_30
action_95 (64) = happyShift action_31
action_95 (5) = happyGoto action_96
action_95 (6) = happyGoto action_4
action_95 (7) = happyGoto action_5
action_95 (8) = happyGoto action_6
action_95 (9) = happyGoto action_7
action_95 (10) = happyGoto action_8
action_95 (11) = happyGoto action_9
action_95 (12) = happyGoto action_10
action_95 (13) = happyGoto action_11
action_95 (17) = happyGoto action_12
action_95 (18) = happyGoto action_13
action_95 (19) = happyGoto action_14
action_95 (21) = happyGoto action_15
action_95 (26) = happyGoto action_16
action_95 (27) = happyGoto action_17
action_95 (28) = happyGoto action_18
action_95 (29) = happyGoto action_19
action_95 _ = happyFail

action_96 (35) = happyShift action_54
action_96 (53) = happyShift action_106
action_96 _ = happyFail

action_97 (49) = happyShift action_105
action_97 _ = happyFail

action_98 _ = happyReduce_26

action_99 (49) = happyShift action_104
action_99 _ = happyFail

action_100 (35) = happyShift action_78
action_100 (22) = happyGoto action_103
action_100 _ = happyFail

action_101 _ = happyReduce_53

action_102 _ = happyReduce_23

action_103 _ = happyReduce_25

action_104 (35) = happyShift action_78
action_104 (22) = happyGoto action_108
action_104 _ = happyFail

action_105 (35) = happyShift action_78
action_105 (22) = happyGoto action_107
action_105 _ = happyFail

action_106 _ = happyReduce_49

action_107 (59) = happyShift action_94
action_107 (15) = happyGoto action_109
action_107 _ = happyReduce_29

action_108 _ = happyReduce_32

action_109 _ = happyReduce_30

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
happyReduction_15 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn6
		 (Expression happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Assert happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn20  happy_var_3)
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
happyReduction_22 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ClassDef happy_var_2 [] happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 10 happyReduction_23
happyReduction_23 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
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
happyReduction_25 ((HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Def happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 6 13 happyReduction_26
happyReduction_26 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (If ((IfClause happy_var_2 happy_var_4):happy_var_5) happy_var_6
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_0  14 happyReduction_27
happyReduction_27  =  HappyAbsSyn14
		 ([]
	)

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 15 happyReduction_29
happyReduction_29 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ([IfClause happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((IfClause happy_var_2 happy_var_4):happy_var_5
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_0  16 happyReduction_31
happyReduction_31  =  HappyAbsSyn16
		 ([]
	)

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_3)
	_
	_
	 =  HappyAbsSyn16
		 (happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn17
		 (Pass
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (Return $ Constant None
	)

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Return happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 19 happyReduction_36
happyReduction_36 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  21 happyReduction_38
happyReduction_38 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (ArithOp Mul) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  21 happyReduction_39
happyReduction_39 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (ArithOp Div) happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (ArithOp Add) happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (ArithOp Sub) happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp Eq) happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp NotEq) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp LessThan) happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp LessThanEq) happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  21 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp GreaterThan) happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  21 happyReduction_47
happyReduction_47 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (BinOp (BoolOp GreaterThanEq) happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 22 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_0  23 happyReduction_50
happyReduction_50  =  HappyAbsSyn23
		 ([]
	)

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  24 happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1:happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn26
		 (Variable happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  26 happyReduction_56
happyReduction_56 (HappyTerminal (L.Literal happy_var_1))
	 =  HappyAbsSyn26
		 (Constant happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  28 happyReduction_60
happyReduction_60 (HappyTerminal (L.Identifier happy_var_3))
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (Attribute happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 29 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_0  30 happyReduction_62
happyReduction_62  =  HappyAbsSyn30
		 ([]
	)

happyReduce_63 = happySpecReduce_1  30 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  31 happyReduction_64
happyReduction_64 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  31 happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1:happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyNewToken action sts stk
	= L.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L.EOF -> action 65 65 tk (HappyState action) sts stk;
	L.Identifier happy_dollar_dollar -> cont 33;
	L.Literal happy_dollar_dollar -> cont 34;
	L.Newline -> cont 35;
	L.Punctuation "+" -> cont 36;
	L.Punctuation "-" -> cont 37;
	L.Punctuation "*" -> cont 38;
	L.Punctuation "/" -> cont 39;
	L.Punctuation "==" -> cont 40;
	L.Punctuation "!=" -> cont 41;
	L.Punctuation "<" -> cont 42;
	L.Punctuation "<=" -> cont 43;
	L.Punctuation ">" -> cont 44;
	L.Punctuation ">=" -> cont 45;
	L.Punctuation "." -> cont 46;
	L.Punctuation "(" -> cont 47;
	L.Punctuation ")" -> cont 48;
	L.Punctuation ":" -> cont 49;
	L.Punctuation "," -> cont 50;
	L.Punctuation "=" -> cont 51;
	L.Indent -> cont 52;
	L.Dedent -> cont 53;
	L.Keyword "assert" -> cont 54;
	L.Keyword "break" -> cont 55;
	L.Keyword "class" -> cont 56;
	L.Keyword "continue" -> cont 57;
	L.Keyword "def" -> cont 58;
	L.Keyword "elif" -> cont 59;
	L.Keyword "else" -> cont 60;
	L.Keyword "if" -> cont 61;
	L.Keyword "pass" -> cont 62;
	L.Keyword "return" -> cont 63;
	L.Keyword "while" -> cont 64;
	_ -> happyError' tk
	})

happyError_ 65 tk = happyError' tk
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
