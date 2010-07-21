{-# LANGUAGE EmptyDataDecls #-}
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

class AbsoluteTolerance tolerance where
    absoluteToleranceOf ::
        Fractional value =>
        AbsolutelyApproximateValue tolerance value ->
        value

data Digits n

getDigitsOfAbsoluteTolerance ::
    NaturalNumber n =>
    AbsolutelyApproximateValue (Digits n) value ->
    Int
getDigitsOfAbsoluteTolerance = naturalNumberAsInt . getDigits
  where
    getDigits :: AbsolutelyApproximateValue (Digits n) value -> n
    getDigits _ = undefined

instance NaturalNumber n => AbsoluteTolerance (Digits n) where
    absoluteToleranceOf =
        recip
        .
        fromInteger
        .
        ((10::Integer) ^)
        .
        getDigitsOfAbsoluteTolerance

instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Show (AbsolutelyApproximateValue tolerance value)
  where
    show x =
        show (unwrapAbsolutelyApproximateValue x)
        ++ " +/- " ++
        show (absoluteToleranceOf x)

instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Eq (AbsolutelyApproximateValue tolerance value)
  where
    a == b =
        abs (x - y) <= absoluteToleranceOf a
      where
        x = unwrapAbsolutelyApproximateValue a
        y = unwrapAbsolutelyApproximateValue b

instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Ord (AbsolutelyApproximateValue tolerance value)
  where
    compare a b
      | a == b    = EQ
      | otherwise = compare x y
      where
        x = unwrapAbsolutelyApproximateValue a
        y = unwrapAbsolutelyApproximateValue b
