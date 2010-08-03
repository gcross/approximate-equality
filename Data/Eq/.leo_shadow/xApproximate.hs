-- @+leo-ver=4-thin
-- @+node:gcross.20100730133924.1260:@shadow Data/Eq/Approximate.hs
-- @@language haskell

-- @<< Language extensions >>
-- @+node:gcross.20100730133924.1261:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
-- @-node:gcross.20100730133924.1261:<< Language extensions >>
-- @nl

module Data.Eq.Approximate where

-- @<< Import needed modules >>
-- @+node:gcross.20100730133924.1262:<< Import needed modules >>
import Control.Arrow
import Data.Function

import TypeLevel.NaturalNumber
-- @nonl
-- @-node:gcross.20100730133924.1262:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100730133924.1263:Types
-- @+node:gcross.20100730133924.1264:AbsolutelyApproximateValue
{-|
    The newtype AbsolutelyApproximateValue is a wrapper that can contain an arbitrary value tagged with a tolerance.
-}
newtype AbsolutelyApproximateValue absolute_tolerance value =
        AbsolutelyApproximateValue { unwrapAbsolutelyApproximateValue :: value }

-- @-node:gcross.20100730133924.1264:AbsolutelyApproximateValue
-- @+node:gcross.20100730133924.1267:Digits
data Digits n
-- @nonl
-- @-node:gcross.20100730133924.1267:Digits
-- @-node:gcross.20100730133924.1263:Types
-- @+node:gcross.20100730133924.1265:Classes
-- @+node:gcross.20100730133924.1266:AbsoluteTolerance
class AbsoluteTolerance tolerance where
    absoluteToleranceOf ::
        Fractional value =>
        AbsolutelyApproximateValue tolerance value ->
        value
-- @nonl
-- @-node:gcross.20100730133924.1266:AbsoluteTolerance
-- @-node:gcross.20100730133924.1265:Classes
-- @+node:gcross.20100730133924.1270:Instances
-- @+node:gcross.20100730133924.1273:AbsolutelyApproximateValue
-- @+node:gcross.20100730133924.1272:Show
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
-- @nonl
-- @-node:gcross.20100730133924.1272:Show
-- @+node:gcross.20100730133924.1275:Eq
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
-- @-node:gcross.20100730133924.1275:Eq
-- @+node:gcross.20100730133924.1276:Ord
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
-- @-node:gcross.20100730133924.1276:Ord
-- @+node:gcross.20100802173247.1280:Enum
instance Enum value => Enum (AbsolutelyApproximateValue tolerance value) where
    succ = liftAAV1 succ
    pred = liftAAV1 pred
    toEnum = wrapAAV . toEnum
    fromEnum = unwrapAAVAndApply fromEnum
    enumFrom = map wrapAAV . enumFrom . unwrapAAV
    enumFromThen a b = map wrapAAV (enumFromThen (unwrapAAV a) (unwrapAAV b))
    enumFromTo a b = map wrapAAV (enumFromTo (unwrapAAV a) (unwrapAAV b))
    enumFromThenTo a b c = map wrapAAV (enumFromThenTo (unwrapAAV a) (unwrapAAV b) (unwrapAAV c))
-- @-node:gcross.20100802173247.1280:Enum
-- @+node:gcross.20100802173247.1284:Num
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
-- @-node:gcross.20100802173247.1284:Num
-- @+node:gcross.20100802173247.1286:Real
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ,Real value
    ) => Real (AbsolutelyApproximateValue tolerance value)
  where
    toRational = unwrapAAVAndApply toRational
-- @-node:gcross.20100802173247.1286:Real
-- @+node:gcross.20100802173247.1287:Integral
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
-- @-node:gcross.20100802173247.1287:Integral
-- @+node:gcross.20100802173247.1288:Fractional
instance
    (AbsoluteTolerance tolerance
    ,Ord value
    ,Fractional value
    ) => Fractional (AbsolutelyApproximateValue tolerance value)
  where
    (/) = liftAAV2 (/)
    recip = liftAAV1 recip
    fromRational = wrapAAV . fromRational
-- @-node:gcross.20100802173247.1288:Fractional
-- @+node:gcross.20100802173247.1290:Floating
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
-- @-node:gcross.20100802173247.1290:Floating
-- @+node:gcross.20100802173247.1293:RealFrac
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
-- @-node:gcross.20100802173247.1293:RealFrac
-- @+node:gcross.20100802173247.1295:RealFloat
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
-- @-node:gcross.20100802173247.1295:RealFloat
-- @-node:gcross.20100730133924.1273:AbsolutelyApproximateValue
-- @+node:gcross.20100730133924.1274:Digits
-- @+node:gcross.20100730133924.1271:AbsoluteTolerance
instance NaturalNumber n => AbsoluteTolerance (Digits n) where
    absoluteToleranceOf =
        recip
        .
        fromInteger
        .
        ((10::Integer) ^)
        .
        getDigitsOfAbsoluteTolerance
-- @-node:gcross.20100730133924.1271:AbsoluteTolerance
-- @-node:gcross.20100730133924.1274:Digits
-- @-node:gcross.20100730133924.1270:Instances
-- @+node:gcross.20100730133924.1268:Functions
-- @+node:gcross.20100730133924.1269:getDigitsOfAbsoluteTolerance
getDigitsOfAbsoluteTolerance ::
    NaturalNumber n =>
    AbsolutelyApproximateValue (Digits n) value ->
    Int
getDigitsOfAbsoluteTolerance = naturalNumberAsInt . getDigits
  where
    getDigits :: AbsolutelyApproximateValue (Digits n) value -> n
    getDigits _ = undefined
-- @-node:gcross.20100730133924.1269:getDigitsOfAbsoluteTolerance
-- @-node:gcross.20100730133924.1268:Functions
-- @+node:gcross.20100802173247.1283:Internal
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
-- @-node:gcross.20100802173247.1283:Internal
-- @-others
-- @-node:gcross.20100730133924.1260:@shadow Data/Eq/Approximate.hs
-- @-leo
