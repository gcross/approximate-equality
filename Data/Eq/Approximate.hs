
{-# LANGUAGE EmptyDataDecls #-}

module Data.Eq.Approximate
    (AbsolutelyApproximateValue(..)
    ,RelativelyApproximateValue(..)
    ,AbsoluteTolerance
    ,RelativeTolerance
    ,ZeroTolerance
    ,Digits(..)
    ,toleranceFromDigits
    ,getDigitsOfAbsoluteTolerance
    ,getDigitsOfRelativeTolerance
    ,getDigitsOfZeroTolerance
    ) where

import Control.Arrow
import Data.Function
import Text.Printf

import TypeLevel.NaturalNumber

{-|
    The newtype AbsolutelyApproximateValue is a wrapper that can contain an arbitrary value tagged with a tolerance.
-}
newtype AbsolutelyApproximateValue absolute_tolerance value =
        AbsolutelyApproximateValue { unwrapAbsolutelyApproximateValue :: value }

{-|
    The newtype RelativelyApproximateValue is a wrapper that can contain an arbitrary value tagged with a tolerance.
-}
newtype RelativelyApproximateValue zero_tolerance relative_tolerance value =
        RelativelyApproximateValue { unwrapRelativelyApproximateValue :: value }
data Digits n
class AbsoluteTolerance tolerance where
    absoluteToleranceOf ::
        Fractional value =>
        AbsolutelyApproximateValue tolerance value ->
        value
class RelativeTolerance relative_tolerance where
    relativeToleranceOf ::
        Fractional value =>
        RelativelyApproximateValue zero_tolerance relative_tolerance value ->
        value
class ZeroTolerance zero_tolerance where
    zeroToleranceOf ::
        Fractional value =>
        RelativelyApproximateValue zero_tolerance relative_tolerance value ->
        value
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Show (AbsolutelyApproximateValue tolerance value)
  where
    show x =
        show (unwrapAAV x)
        ++ " +/- " ++
        show (absoluteToleranceOf x)
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Eq (AbsolutelyApproximateValue tolerance value)
  where
    a == b = abs (unwrapAAV a - unwrapAAV b) <= absoluteToleranceOf a
instance (AbsoluteTolerance tolerance
         ,Ord value
         ,Fractional value
         ) =>
         Ord (AbsolutelyApproximateValue tolerance value)
  where
    compare a b
      | a == b    = EQ
      | otherwise = (compare `on` unwrapAAV) a b
instance Enum value => Enum (AbsolutelyApproximateValue tolerance value) where
    succ = liftAAV1 succ
    pred = liftAAV1 pred
    toEnum = wrapAAV . toEnum
    fromEnum = unwrapAAVAndApply fromEnum
    enumFrom = map wrapAAV . enumFrom . unwrapAAV
    enumFromThen a b = map wrapAAV (enumFromThen (unwrapAAV a) (unwrapAAV b))
    enumFromTo a b = map wrapAAV (enumFromTo (unwrapAAV a) (unwrapAAV b))
    enumFromThenTo a b c = map wrapAAV (enumFromThenTo (unwrapAAV a) (unwrapAAV b) (unwrapAAV c))
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ) => Num (AbsolutelyApproximateValue tolerance value)
  where
    (+) = liftAAV2 (+)
    (*) = liftAAV2 (*)
    (-) = liftAAV2 (-)
    negate = liftAAV1 negate
    abs = liftAAV1 abs
    signum = liftAAV1 signum
    fromInteger = wrapAAV . fromInteger
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ,Real value
    ) => Real (AbsolutelyApproximateValue tolerance value)
  where
    toRational = unwrapAAVAndApply toRational
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ,Integral value
    ) => Integral (AbsolutelyApproximateValue tolerance value)
  where
    quot = liftAAV2 quot
    rem = liftAAV2 rem
    div = liftAAV2 div
    mod = liftAAV2 mod
    quotRem a b = (wrapAAV *** wrapAAV) $ (quotRem `on` unwrapAAV) a b
    divMod a b = (wrapAAV *** wrapAAV) $ (divMod `on` unwrapAAV) a b
    toInteger = toInteger . unwrapAAV
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ) => Fractional (AbsolutelyApproximateValue tolerance value)
  where
    (/) = liftAAV2 (/)
    recip = liftAAV1 recip
    fromRational = wrapAAV . fromRational
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Floating value
    ) => Floating (AbsolutelyApproximateValue tolerance value)
  where
    pi = wrapAAV pi
    exp = liftAAV1 exp
    sqrt = liftAAV1 sqrt
    log = liftAAV1 log
    (**) = liftAAV2 (**)
    logBase = liftAAV2 logBase
    sin = liftAAV1 sin
    tan = liftAAV1 tan
    cos = liftAAV1 cos
    asin = liftAAV1 asin
    atan = liftAAV1 atan
    acos = liftAAV1 acos
    sinh = liftAAV1 sinh
    tanh = liftAAV1 tanh
    cosh = liftAAV1 cosh
    asinh = liftAAV1 asinh
    atanh = liftAAV1 atanh
    acosh = liftAAV1 acosh
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,RealFrac value
    ) => RealFrac (AbsolutelyApproximateValue tolerance value)
  where
    properFraction = second wrapAAV . unwrapAAVAndApply properFraction
    truncate = unwrapAAVAndApply truncate
    round = unwrapAAVAndApply round
    ceiling = unwrapAAVAndApply ceiling
    floor = unwrapAAVAndApply floor
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,RealFloat value
    ) => RealFloat (AbsolutelyApproximateValue tolerance value)
  where
    floatRadix = unwrapAAVAndApply floatRadix
    floatDigits = unwrapAAVAndApply floatDigits
    floatRange = unwrapAAVAndApply floatRange
    decodeFloat = unwrapAAVAndApply decodeFloat
    encodeFloat x y = wrapAAV (encodeFloat x y)
    exponent = unwrapAAVAndApply exponent
    significand = liftAAV1 significand
    scaleFloat i = liftAAV1 (scaleFloat i)
    isNaN = unwrapAAVAndApply isNaN
    isInfinite = unwrapAAVAndApply isInfinite
    isDenormalized = unwrapAAVAndApply isDenormalized
    isNegativeZero = unwrapAAVAndApply isNegativeZero
    isIEEE = unwrapAAVAndApply isIEEE
    atan2 = liftAAV2 atan2
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ) =>
         Show (RelativelyApproximateValue zerotol reltol value)
  where
    show a@(RelativelyApproximateValue x) =
        if a == 0
            then "0 +/- " ++ show (zeroToleranceOf a)
            else show x ++ " +/- " ++ show (x * relativeToleranceOf a * 100)
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ) =>
         Eq (RelativelyApproximateValue zerotol reltol value)
  where
    a == b =
        case (unwrapRAV a,unwrapRAV b) of
            (0,0) -> True
            (0,y) -> abs y <= zerotol
            (x,0) -> abs x <= zerotol
            (x,y)
              | abs x <= zerotol && abs y <= zerotol -> True
              | otherwise -> abs (x - y) / (abs x + abs y) * 2 <= reltol
      where
        zerotol = zeroToleranceOf a
        reltol = relativeToleranceOf a
instance (ZeroTolerance zerotol
         ,RelativeTolerance reltol
         ,Ord value
         ,Fractional value
         ) =>
         Ord (RelativelyApproximateValue zerotol reltol value)
  where
    compare a b
      | a == b    = EQ
      | otherwise = (compare `on` unwrapRAV) a b
instance Enum value => Enum (RelativelyApproximateValue zerotol reltol value) where
    succ = liftRAV1 succ
    pred = liftRAV1 pred
    toEnum = wrapRAV . toEnum
    fromEnum = unwrapRAVAndApply fromEnum
    enumFrom = map wrapRAV . enumFrom . unwrapRAV
    enumFromThen a b = map wrapRAV (enumFromThen (unwrapRAV a) (unwrapRAV b))
    enumFromTo a b = map wrapRAV (enumFromTo (unwrapRAV a) (unwrapRAV b))
    enumFromThenTo a b c = map wrapRAV (enumFromThenTo (unwrapRAV a) (unwrapRAV b) (unwrapRAV c))
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ) => Num (RelativelyApproximateValue zerotol reltol value)
  where
    (+) = liftRAV2 (+)
    (*) = liftRAV2 (*)
    (-) = liftRAV2 (-)
    negate = liftRAV1 negate
    abs = liftRAV1 abs
    signum = liftRAV1 signum
    fromInteger = wrapRAV . fromInteger
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ,Real value
    ) => Real (RelativelyApproximateValue zerotol reltol value)
  where
    toRational = unwrapRAVAndApply toRational
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ,Integral value
    ) => Integral (RelativelyApproximateValue zerotol reltol value)
  where
    quot = liftRAV2 quot
    rem = liftRAV2 rem
    div = liftRAV2 div
    mod = liftRAV2 mod
    quotRem a b = (wrapRAV *** wrapRAV) $ (quotRem `on` unwrapRAV) a b
    divMod a b = (wrapRAV *** wrapRAV) $ (divMod `on` unwrapRAV) a b
    toInteger = toInteger . unwrapRAV
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Fractional value
    ) => Fractional (RelativelyApproximateValue zerotol reltol value)
  where
    (/) = liftRAV2 (/)
    recip = liftRAV1 recip
    fromRational = wrapRAV . fromRational
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,Floating value
    ) => Floating (RelativelyApproximateValue zerotol reltol value)
  where
    pi = wrapRAV pi
    exp = liftRAV1 exp
    sqrt = liftRAV1 sqrt
    log = liftRAV1 log
    (**) = liftRAV2 (**)
    logBase = liftRAV2 logBase
    sin = liftRAV1 sin
    tan = liftRAV1 tan
    cos = liftRAV1 cos
    asin = liftRAV1 asin
    atan = liftRAV1 atan
    acos = liftRAV1 acos
    sinh = liftRAV1 sinh
    tanh = liftRAV1 tanh
    cosh = liftRAV1 cosh
    asinh = liftRAV1 asinh
    atanh = liftRAV1 atanh
    acosh = liftRAV1 acosh
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,RealFrac value
    ) => RealFrac (RelativelyApproximateValue zerotol reltol value)
  where
    properFraction = second wrapRAV . unwrapRAVAndApply properFraction
    truncate = unwrapRAVAndApply truncate
    round = unwrapRAVAndApply round
    ceiling = unwrapRAVAndApply ceiling
    floor = unwrapRAVAndApply floor
instance
    (ZeroTolerance zerotol
    ,RelativeTolerance reltol
    ,Ord value
    ,RealFloat value
    ) => RealFloat (RelativelyApproximateValue zerotol reltol value)
  where
    floatRadix = unwrapRAVAndApply floatRadix
    floatDigits = unwrapRAVAndApply floatDigits
    floatRange = unwrapRAVAndApply floatRange
    decodeFloat = unwrapRAVAndApply decodeFloat
    encodeFloat x y = wrapRAV (encodeFloat x y)
    exponent = unwrapRAVAndApply exponent
    significand = liftRAV1 significand
    scaleFloat i = liftRAV1 (scaleFloat i)
    isNaN = unwrapRAVAndApply isNaN
    isInfinite = unwrapRAVAndApply isInfinite
    isDenormalized = unwrapRAVAndApply isDenormalized
    isNegativeZero = unwrapRAVAndApply isNegativeZero
    isIEEE = unwrapRAVAndApply isIEEE
    atan2 = liftRAV2 atan2
instance NaturalNumber n => AbsoluteTolerance (Digits n) where
    absoluteToleranceOf =
        toleranceFromDigits
        .
        getDigitsOfAbsoluteTolerance
instance NaturalNumber n => RelativeTolerance (Digits n) where
    relativeToleranceOf =
        toleranceFromDigits
        .
        getDigitsOfRelativeTolerance
instance NaturalNumber n => ZeroTolerance (Digits n) where
    zeroToleranceOf =
        toleranceFromDigits
        .
        getDigitsOfZeroTolerance
toleranceFromDigits :: Fractional value => Int -> value
toleranceFromDigits =
    recip
    .
    fromInteger
    .
    ((10::Integer) ^)
getDigitsOfAbsoluteTolerance ::
    NaturalNumber n =>
    AbsolutelyApproximateValue (Digits n) value ->
    Int
getDigitsOfAbsoluteTolerance = naturalNumberAsInt . getDigits
  where
    getDigits :: AbsolutelyApproximateValue (Digits n) value -> n
    getDigits _ = undefined
getDigitsOfRelativeTolerance ::
    NaturalNumber n =>
    RelativelyApproximateValue zero_tolerance (Digits n) value ->
    Int
getDigitsOfRelativeTolerance = naturalNumberAsInt . getDigits
  where
    getDigits :: RelativelyApproximateValue zero_tolerance (Digits n) value -> n
    getDigits _ = undefined
getDigitsOfZeroTolerance ::
    NaturalNumber n =>
    RelativelyApproximateValue (Digits n) relative_tolerance value ->
    Int
getDigitsOfZeroTolerance = naturalNumberAsInt . getDigits
  where
    getDigits :: RelativelyApproximateValue (Digits n) relative_tolerance value -> n
    getDigits _ = undefined
{-# INLINE wrapAAV #-}
{-# INLINE unwrapAAV #-}
{-# INLINE liftAAV1 #-}
{-# INLINE liftAAV2 #-}
{-# INLINE unwrapAAVAndApply #-}
wrapAAV = AbsolutelyApproximateValue
unwrapAAV = unwrapAbsolutelyApproximateValue
liftAAV1 f = wrapAAV . f . unwrapAAV
liftAAV2 f a b = wrapAAV (f (unwrapAAV a) (unwrapAAV b))
unwrapAAVAndApply f = f . unwrapAAV

{-# INLINE wrapRAV #-}
{-# INLINE unwrapRAV #-}
{-# INLINE liftRAV1 #-}
{-# INLINE liftRAV2 #-}
{-# INLINE unwrapRAVAndApply #-}
wrapRAV = RelativelyApproximateValue
unwrapRAV = unwrapRelativelyApproximateValue
liftRAV1 f = wrapRAV . f . unwrapRAV
liftRAV2 f a b = wrapRAV (f (unwrapRAV a) (unwrapRAV b))
unwrapRAVAndApply f = f . unwrapRAV
