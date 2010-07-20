{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Eq.Approximate where

import TypeLevel.NaturalNumber

newtype AbsolutelyApproximateValue absolute_tolerance value =
        AbsolutelyApproximateValue { unwrapAbsolutelyApproximateValue :: value }
  deriving
    (Enum
    ,Num
    ,Real
    ,Integral
    ,Fractional
    ,Floating
    ,RealFrac
    ,RealFloat
    )

wrapAbsolutelyApproximateValue ::
    value ->
    AbsolutelyApproximateValue absolute_tolerance value
wrapAbsolutelyApproximateValue = AbsolutelyApproximateValue

absoluteToleranceInDecimalsAsNaturalNumberOf ::
    AbsolutelyApproximateValue absolute_tolerance value ->
    absolute_tolerance
absoluteToleranceInDecimalsAsNaturalNumberOf = undefined

absoluteToleranceInDecimalsOf ::
    NaturalNumber absolute_tolerance =>
    AbsolutelyApproximateValue absolute_tolerance value ->
    Int
absoluteToleranceInDecimalsOf =
    naturalNumberOf
    .
    absoluteToleranceInDecimalsAsNaturalNumberOf

absoluteToleranceOf ::
    (NaturalNumber absolute_tolerance
    ,Fractional value
    ) =>
    AbsolutelyApproximateValue absolute_tolerance value ->
    value
absoluteToleranceOf =
    recip
    .
    fromIntegral
    .
    ((10::Integer) ^)
    .
    absoluteToleranceInDecimalsOf

instance (NaturalNumber absolute_tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Show (AbsolutelyApproximateValue absolute_tolerance value)
  where
    show x =
        show (unwrapAbsolutelyApproximateValue x)
        ++ " +/- " ++
        show (absoluteToleranceOf x)

instance (NaturalNumber absolute_tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Eq (AbsolutelyApproximateValue absolute_tolerance value)
  where
    a@(AbsolutelyApproximateValue x) == (AbsolutelyApproximateValue y) =
        abs (x - y) <= absoluteToleranceOf a
    a@(AbsolutelyApproximateValue x) /= (AbsolutelyApproximateValue y) =
        abs (x - y) > absoluteToleranceOf a


instance (NaturalNumber absolute_tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Ord (AbsolutelyApproximateValue absolute_tolerance value)
  where
    compare a@(AbsolutelyApproximateValue x) b@(AbsolutelyApproximateValue y)
      | a == b    = EQ
      | otherwise = compare x y
