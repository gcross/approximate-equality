-- @+leo-ver=4-thin
-- @+node:gcross.20100802173247.1335:@shadow test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100802173247.1336:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100802173247.1336:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100802173247.1337:<< Import needed modules >>
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import TypeLevel.NaturalNumber

import Data.Eq.Approximate
-- @nonl
-- @-node:gcross.20100802173247.1337:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100802173247.1338:Generators
instance Arbitrary value => Arbitrary (AbsolutelyApproximateValue tolerance value) where
    arbitrary = fmap AbsolutelyApproximateValue arbitrary

instance Arbitrary value => Arbitrary (RelativelyApproximateValue zerotol reltol value) where
    arbitrary = fmap RelativelyApproximateValue arbitrary
-- @-node:gcross.20100802173247.1338:Generators
-- @+node:gcross.20100802173247.1339:Tolerance aliases
-- @+node:gcross.20100802173247.1340:A
type A = AbsolutelyApproximateValue (Digits N5) Double
wrapA :: Double -> A
wrapA = AbsolutelyApproximateValue
unwrapA :: A -> Double
unwrapA = unwrapAbsolutelyApproximateValue
-- @-node:gcross.20100802173247.1340:A
-- @+node:gcross.20100802173247.1342:R
type R = RelativelyApproximateValue (Digits N7) (Digits N5) Double
wrapR :: Double -> R
wrapR = RelativelyApproximateValue
unwrapR :: R -> Double
unwrapR = unwrapRelativelyApproximateValue
-- @-node:gcross.20100802173247.1342:R
-- @-node:gcross.20100802173247.1339:Tolerance aliases
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100802173247.1343:<< Tests >>
    -- @+others
    -- @+node:gcross.20100802173247.1344:Absolutely approximate values
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
    -- @-node:gcross.20100802173247.1344:Absolutely approximate values
    -- @+node:gcross.20100802173247.1346:Relatively approximate values
    ,testGroup "Relatively approximate values"
        [testGroup "Num operations"
            [testProperty "+" $ \a b -> wrapR (unwrapR a + unwrapR b) == a + b
            ,testProperty "-" $ \a b -> wrapR (unwrapR a - unwrapR b) == a - b
            ,testProperty "*" $ \a b -> wrapR (unwrapR a * unwrapR b) == a * b
            ] 
        ,testGroup "Eq operations"
            [testGroup "Non-zero"
                [testProperty "Inside range" $ \a -> a /= 0 ==> a == a + (a * wrapR 1e-6)
                ,testProperty "Outside range" $ \a -> a /= 0 ==> a /= a + (a * wrapR 1e-4)
                ]
            ,testGroup "Zero"
                [testCase "Inside range" $
                    assertBool "Is the value equal to zero within the tolerance?" $
                        0 == wrapR 1e-8 && wrapR 1e-8 == 0
                ,testCase "Outside range" $
                    assertBool "Is the value not equal to zero within the tolerance?" $
                        0 /= wrapR 1e-6 && wrapR 1e-6 /= 0
                ,testCase "Both inside range" $
                    assertBool "Is the value equal to zero within the tolerance?" $
                        wrapR 1e-20 == wrapR 1e-8 && wrapR 1e-8 == wrapR 1e-20
                ]  
            ]      
        ]
    -- @-node:gcross.20100802173247.1346:Relatively approximate values
    -- @-others
    -- @nonl
    -- @-node:gcross.20100802173247.1343:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100802173247.1335:@shadow test.hs
-- @-leo
