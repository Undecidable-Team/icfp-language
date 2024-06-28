module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Types
import Eval

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ evalTests ]

evalTests = testGroup "Eval"
  [ strToIntTests
  , intToStrTests
  , s2iRoundTests
  ]

strToIntTests = testGroup "strToInt"
  [ testCase "spec: S4%34" $ strToInt "S4%34" @?= 15818151 ]

intToStrTests = testGroup "intToStr"
  [ testCase "spec: I4%34" $ intToStr 15818151 @?= "test" ]

s2iRoundTests = testGroup "s2i roundtrip"
  [ testProperty "strToInt . intToStr == id" $
    \i -> strToInt (intToStr i) == i
  ]
