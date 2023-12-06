{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

import Tut3

main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\nTutorial 03 Tests\n"
            [ insertTests
            , lookupTests
            , transposeTests
           ]
        ]


insertTests :: TF.Test
insertTests
 = testGroup "\nPart 1 - Insertions\n"
    [ testCase "What?" (1+1  @?= (3::Int))
    ]

lookupTests :: TF.Test
lookupTests
 = testGroup "\nPart 2 - Lookups\n"
    [ testCase "Where?" (2+2  @?= (5::Int))
    ]

transposeTests :: TF.Test
transposeTests
 = testGroup "\nPart 3 - Transpose\n"
    [ testCase "Why?" (42+99   @?= (0::Int))
    ]
