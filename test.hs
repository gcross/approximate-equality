{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import TypeLevel.NaturalNumber

import Data.Eq.Approximate

instance Arbitrary value => Arbitrary (AbsolutelyApproximateValue tolerance value) where
    arbitrary = fmap AbsolutelyApproximateValue arbitrary

type A = AbsolutelyApproximateValue N5 Double
wrapA :: Double -> A
wrapA = wrapAbsolutelyApproximateValue
unwrapA :: A -> Double
unwrapA = unwrapAbsolutelyApproximateValue

main = defaultMain
    [testGroup "Absolutely approximate values"
        [testGroup "Num operations"
            [testProperty "+" $ \a b -> wrapA (unwrapA a + unwrapA b) == a + b
            ,testProperty "-" $ \a b -> wrapA (unwrapA a - unwrapA b) == a - b
            ,testProperty "*" $ \a b -> wrapA (unwrapA a * unwrapA b) == a * b
            ] 
        ,testGroup "Eq operations"
            [testProperty "Inside range" $ \a -> a == a + wrapA 1e-6
            ,testProperty "Outside range" $ \a -> a /= a + wrapA 1e-4
            ]
        ]
    ]
