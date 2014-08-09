{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser(parse) where
import qualified Lexer as L
import Control.Monad.Error

import Language
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

newtype HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: t10 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: t11 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: t12 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: t13 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: t14 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: t15 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: t16 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: t17 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: t18 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: t19 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: t20 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: t21 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: t22 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: t23 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: t24 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: t25 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: t26 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: t27 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: t28 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: t29 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: t30 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: t31 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: t32 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: t33 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: t34 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> t34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyInTok :: (L.Token) -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34) -> (L.Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x00\x00\x80\x00\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x00\x00\x20\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x01\x00\x40\x00\x00\x00\x91\x00\x00\x00\x90\x00\x40\x00\x00\x00\x40\x00\x40\x00\xc7\x00\x00\x00\x00\x00\xdd\x00\xb9\x00\x81\x00\xfa\xff\x00\x00\x8a\x00\x40\x00\x88\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x40\x00\x0c\x00\x00\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\xdd\x00\x78\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x86\x00\x76\x00\x77\x00\x73\x00\x73\x00\x00\x00\x63\x00\x59\x00\x5f\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x55\x00\x40\x00\x00\x00\x00\x00\x6b\x00\x5d\x00\x4c\x00\x43\x00\x00\x00\x40\x00\x0c\x00\xfe\xff\x00\x00\x00\x00\xab\x00\x00\x00\x4b\x00\x57\x00\x00\x00\x00\x00\x00\x00\x57\x00\x57\x00\x0c\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\xe7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x01\x00\x00\xe3\x00\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\xc2\x00\x31\x01\x28\x01\x24\x01\x1b\x01\x17\x01\x0e\x01\x0a\x01\x01\x01\xfd\x00\xf4\x00\x93\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x42\x00\xf0\xff\x41\x00\x37\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\x00\x00\x00\x00\x30\x00\x06\x00\x00\x00\x35\x00\x00\x00\xf0\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf8\xff\x62\x00\x00\x00\x00\x00\xfb\xff\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfe\xff\x00\x00\x00\x00\xfd\xff\xf7\xff\xf1\xff\xf2\xff\xf0\xff\xef\xff\xee\xff\xed\xff\xec\xff\xeb\xff\xea\xff\xe9\xff\xe8\xff\xc1\xff\xca\xff\xc0\xff\xbf\xff\xc3\xff\xc2\xff\xf3\xff\x00\x00\xe5\xff\x00\x00\xe2\xff\x00\x00\x00\x00\xd9\xff\xd8\xff\x00\x00\x00\x00\xc3\xff\xd7\xff\xd5\xff\x00\x00\x00\x00\x00\x00\xe7\xff\xf4\xff\x00\x00\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xf6\xff\xcb\xff\xcc\xff\xcd\xff\xce\xff\xcf\xff\xd0\xff\xd3\xff\xd4\xff\xd1\xff\xd2\xff\xb8\xff\x00\x00\xbb\xff\xba\xff\xbe\xff\xe6\xff\xc8\xff\x00\x00\xc8\xff\x00\x00\x00\x00\xd6\xff\x00\x00\xdf\xff\x00\x00\xc7\xff\xc6\xff\xc4\xff\xe4\xff\x00\x00\x00\x00\xbd\xff\xb9\xff\x00\x00\x00\x00\x00\x00\xdb\xff\xde\xff\x00\x00\xf9\xff\x00\x00\xfa\xff\xf8\xff\x00\x00\xe0\xff\x00\x00\x00\x00\xc5\xff\xe3\xff\xe1\xff\x00\x00\x00\x00\xfb\xff\xc9\xff\xfc\xff\xdd\xff\xda\xff\xdc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x02\x00\x03\x00\x15\x00\x16\x00\x17\x00\x0d\x00\x0f\x00\x13\x00\x11\x00\x14\x00\x01\x00\x02\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x15\x00\x1d\x00\x1e\x00\x14\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x0c\x00\x0d\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x0e\x00\x0f\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x15\x00\x16\x00\x17\x00\x0f\x00\x10\x00\x11\x00\x14\x00\x13\x00\x01\x00\x02\x00\x0e\x00\x14\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x02\x00\x14\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x1b\x00\x14\x00\x14\x00\x0f\x00\x10\x00\x11\x00\x03\x00\x13\x00\x11\x00\x11\x00\x01\x00\x1c\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\x10\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x03\x00\x10\x00\x12\x00\x0f\x00\x10\x00\x11\x00\x1b\x00\x13\x00\x03\x00\x14\x00\x01\x00\x03\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\x10\x00\x01\x00\x0f\x00\x10\x00\x11\x00\x03\x00\x13\x00\x12\x00\x0f\x00\x01\x00\x01\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x13\x00\x03\x00\x21\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x13\x00\xff\xff\xff\xff\x11\x00\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x12\x00\x13\x00\xff\xff\xff\xff\x12\x00\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\x13\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x6d\x00\x15\x00\x16\x00\x17\x00\x50\x00\x51\x00\x52\x00\x71\x00\x49\x00\x42\x00\x4a\x00\x6f\x00\x15\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x6e\x00\x58\x00\x45\x00\x70\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x67\x00\x52\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x5c\x00\x5d\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x02\x00\x2b\x00\x2c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x55\x00\x51\x00\x52\x00\x0c\x00\x0d\x00\x0e\x00\x69\x00\x0f\x00\x22\x00\x16\x00\x64\x00\x68\x00\x10\x00\x11\x00\x12\x00\x13\x00\x60\x00\x61\x00\x4d\x00\x62\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x5f\x00\x4f\x00\x54\x00\x0c\x00\x0d\x00\x0e\x00\x4f\x00\x0f\x00\x6b\x00\x67\x00\x54\x00\x66\x00\x10\x00\x11\x00\x12\x00\x13\x00\x6e\x00\x5a\x00\x62\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x4f\x00\x5c\x00\x5b\x00\x0c\x00\x0d\x00\x0e\x00\x5f\x00\x0f\x00\x4f\x00\x60\x00\x54\x00\x4f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x28\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x54\x00\x58\x00\x47\x00\x0c\x00\x0d\x00\x0e\x00\x37\x00\x0f\x00\x57\x00\x4b\x00\x26\x00\x27\x00\x10\x00\x11\x00\x12\x00\x13\x00\x37\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x2a\x00\x37\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x42\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x43\x00\x44\x00\x45\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x47\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x22\x00\x23\x00\x00\x00\x00\x00\x27\x00\x23\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x63\x00\x00\x00\x00\x00\x00\x00\x38\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x39\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x40\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x41\x00\x00\x00\x00\x00\x00\x00\x20\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 71) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71)
	]

happy_n_terms = 34 :: Int
happy_n_nonterms = 31 :: Int

happyReduce_1 = happySpecReduce_0  0# happyReduction_1
happyReduction_1  =  happyIn4
		 ([]
	)

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (happy_var_1
	)}

happyReduce_3 = happySpecReduce_3  1# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_4 = happySpecReduce_2  1# happyReduction_4
happyReduction_4 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

happyReduce_6 = happySpecReduce_0  1# happyReduction_6
happyReduction_6  =  happyIn5
		 ([]
	)

happyReduce_7 = happySpecReduce_1  2# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (happy_var_1
	)}

happyReduce_8 = happySpecReduce_1  3# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ([happy_var_1]
	)}

happyReduce_9 = happySpecReduce_3  3# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_11 = happySpecReduce_2  3# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (happy_var_2
	)}

happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 happy_x_1
	 =  happyIn7
		 ([]
	)

happyReduce_13 = happySpecReduce_1  4# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_15 = happySpecReduce_1  4# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  4# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  4# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_18 = happySpecReduce_1  4# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_19 = happySpecReduce_1  4# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  4# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  4# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_22 = happySpecReduce_1  4# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  4# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (Expression happy_var_1
	)}

happyReduce_24 = happySpecReduce_2  5# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (Assert happy_var_2
	)}

happyReduce_25 = happySpecReduce_3  6# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Identifier happy_var_1) -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (Assignment (Variable happy_var_1) happy_var_3
	)}}

happyReduce_26 = happySpecReduce_1  7# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn11
		 (Break
	)

happyReduce_27 = happyReduce 4# 8# happyReduction_27
happyReduction_27 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L.Identifier happy_var_2) -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	happyIn12
		 (ClassDef happy_var_2 [] happy_var_4
	) `HappyStk` happyRest}}

happyReduce_28 = happyReduce 6# 8# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L.Identifier happy_var_2) -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOut24 happy_x_6 of { happy_var_6 -> 
	happyIn12
		 (ClassDef happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_29 = happySpecReduce_1  9# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn13
		 (Continue
	)

happyReduce_30 = happyReduce 7# 10# happyReduction_30
happyReduction_30 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (L.Identifier happy_var_2) -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOut24 happy_x_7 of { happy_var_7 -> 
	happyIn14
		 (Def happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_31 = happyReduce 6# 11# happyReduction_31
happyReduction_31 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	happyIn15
		 (If ((IfClause happy_var_2 happy_var_4):happy_var_5) happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_32 = happySpecReduce_0  12# happyReduction_32
happyReduction_32  =  happyIn16
		 ([]
	)

happyReduce_33 = happySpecReduce_1  12# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_34 = happyReduce 4# 13# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 ([IfClause happy_var_2 happy_var_4]
	) `HappyStk` happyRest}}

happyReduce_35 = happyReduce 5# 13# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	happyIn17
		 ((IfClause happy_var_2 happy_var_4):happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_36 = happySpecReduce_0  14# happyReduction_36
happyReduction_36  =  happyIn18
		 ([]
	)

happyReduce_37 = happySpecReduce_3  14# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (happy_var_3
	)}

happyReduce_38 = happySpecReduce_1  15# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn19
		 (Pass
	)

happyReduce_39 = happySpecReduce_1  16# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn20
		 (Return $ Constant None
	)

happyReduce_40 = happySpecReduce_2  16# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (Return happy_var_2
	)}

happyReduce_41 = happyReduce 4# 17# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	happyIn21
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_42 = happySpecReduce_1  18# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  19# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (ArithOp Mul) happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_3  19# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (ArithOp Div) happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_3  19# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (ArithOp Add) happy_var_1 happy_var_3
	)}}

happyReduce_46 = happySpecReduce_3  19# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (ArithOp Sub) happy_var_1 happy_var_3
	)}}

happyReduce_47 = happySpecReduce_3  19# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp Eq) happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_3  19# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp NotEq) happy_var_1 happy_var_3
	)}}

happyReduce_49 = happySpecReduce_3  19# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp LessThan) happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_3  19# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp LessThanEq) happy_var_1 happy_var_3
	)}}

happyReduce_51 = happySpecReduce_3  19# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp GreaterThan) happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_3  19# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (BinOp (BoolOp GreaterThanEq) happy_var_1 happy_var_3
	)}}

happyReduce_53 = happySpecReduce_1  19# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_54 = happyReduce 4# 20# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (happy_var_3
	) `HappyStk` happyRest}

happyReduce_55 = happySpecReduce_0  21# happyReduction_55
happyReduction_55  =  happyIn25
		 ([]
	)

happyReduce_56 = happySpecReduce_1  21# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_1  22# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ([happy_var_1]
	)}

happyReduce_58 = happySpecReduce_3  22# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (happy_var_1:happy_var_3
	)}}

happyReduce_59 = happySpecReduce_1  23# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Identifier happy_var_1) -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  24# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Identifier happy_var_1) -> 
	happyIn28
		 (Variable happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  24# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOutTok happy_x_1 of { (L.Literal happy_var_1) -> 
	happyIn28
		 (Constant happy_var_1
	)}

happyReduce_62 = happySpecReduce_1  25# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_64 = happySpecReduce_1  25# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  26# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { (L.Identifier happy_var_3) -> 
	happyIn30
		 (Attribute happy_var_1 happy_var_3
	)}}

happyReduce_66 = happyReduce 4# 27# happyReduction_66
happyReduction_66 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_67 = happySpecReduce_0  28# happyReduction_67
happyReduction_67  =  happyIn32
		 ([]
	)

happyReduce_68 = happySpecReduce_1  28# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_69 = happySpecReduce_1  29# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ([happy_var_1]
	)}

happyReduce_70 = happySpecReduce_3  29# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (happy_var_1:happy_var_3
	)}}

happyReduce_71 = happySpecReduce_1  30# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyNewToken action sts stk
	= L.lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L.EOF -> happyDoAction 33# tk action sts stk;
	L.Identifier happy_dollar_dollar -> cont 1#;
	L.Literal happy_dollar_dollar -> cont 2#;
	L.Newline -> cont 3#;
	L.Punctuation "+" -> cont 4#;
	L.Punctuation "-" -> cont 5#;
	L.Punctuation "*" -> cont 6#;
	L.Punctuation "/" -> cont 7#;
	L.Punctuation "==" -> cont 8#;
	L.Punctuation "!=" -> cont 9#;
	L.Punctuation "<" -> cont 10#;
	L.Punctuation "<=" -> cont 11#;
	L.Punctuation ">" -> cont 12#;
	L.Punctuation ">=" -> cont 13#;
	L.Punctuation "." -> cont 14#;
	L.Punctuation "(" -> cont 15#;
	L.Punctuation ")" -> cont 16#;
	L.Punctuation ":" -> cont 17#;
	L.Punctuation "," -> cont 18#;
	L.Punctuation "=" -> cont 19#;
	L.Indent -> cont 20#;
	L.Dedent -> cont 21#;
	L.Keyword "assert" -> cont 22#;
	L.Keyword "break" -> cont 23#;
	L.Keyword "class" -> cont 24#;
	L.Keyword "continue" -> cont 25#;
	L.Keyword "def" -> cont 26#;
	L.Keyword "elif" -> cont 27#;
	L.Keyword "else" -> cont 28#;
	L.Keyword "if" -> cont 29#;
	L.Keyword "pass" -> cont 30#;
	L.Keyword "return" -> cont 31#;
	L.Keyword "while" -> cont 32#;
	_ -> happyError' tk
	})

happyError_ 33# tk = happyError' tk
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
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq


tokenize code = L.tokenize code
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





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





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

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
