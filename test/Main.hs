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
  [ evalTests
  , parseTests
  ]

parseTests = testGroup "Parse" []

evalTests = testGroup "Eval"
  [ strToIntTests
  , intToStrTests
  -- , s2iRoundTests
  , unaryTests
  , binaryTests
  , ifTests
  , lambdaTests
  ]

strToIntTests = testGroup "strToInt"
  [ testCase "spec: S4%34" $ strToInt "4%34" @?= 15818151 ]

intToStrTests = testGroup "intToStr"
  [ testCase "spec: I4%34" $ intToStr 15818151 @?= "test" ]

s2iRoundTests = testGroup "s2i roundtrip"
  [ testProperty "strToInt . intToStr == id" $
    \i -> strToInt (intToStr i) == i
  ]

unaryTests = testGroup "Unary ops"
  [ testCase "U- I$ -> -3" $
    eval [] (VUnary OpNeg (VInt 3)) @?= VInt (-3)
  , testCase "U! T -> false" $
    eval [] (VUnary OpNot (VBool True)) @?= VBool False
  , testCase "U# S4%34 -> 15818151" $
    eval [] (VUnary OpStringToInt (VString "4%34")) @?= VInt 15818151

    -- TODO: is this test correct?
  , testCase "U$ I4%34 -> test" $
    eval [] (VUnary OpIntToString (VInt 15818151)) @?= VString "4%34"
  ]

binaryTests = testGroup "Binary ops"
  [ testCase "B+ I# I$ -> 5" $
    eval [] (VBinary OpAdd (VInt 3) (VInt 2)) @?= VInt 5
  , testCase "B- I$ I# -> 1" $
    eval [] (VBinary OpSub (VInt 3) (VInt 2)) @?= VInt 1
  , testCase "B* I$ I# -> 1" $
    eval [] (VBinary OpMul (VInt 3) (VInt 2)) @?= VInt 6
  , testCase "B/ U- I( I# -> -3" $
    eval [] (VBinary OpDiv (VUnary OpNeg (VInt 7)) (VInt 2)) @?= VInt (-3)
  , testCase "B% U- I( I# -> -3" $
    eval [] (VBinary OpMod (VUnary OpNeg (VInt 7)) (VInt 2)) @?= VInt (-1)
  , testCase "B< I$ I# -> false" $
    eval [] (VBinary OpLT (VInt 4) (VInt 3)) @?= VBool False
  , testCase "B> I$ I# -> false" $
    eval [] (VBinary OpGT (VInt 4) (VInt 3)) @?= VBool True
  , testCase "B= I$ I# -> false" $
    eval [] (VBinary OpEQ (VInt 4) (VInt 3)) @?= VBool False
  , testCase "B| T F -> true" $
    eval [] (VBinary OpOr (VBool True) (VBool False)) @?= VBool True
  , testCase "B& T F -> true" $
    eval [] (VBinary OpAnd (VBool True) (VBool False)) @?= VBool False
  , testCase "B. S4% S34 -> test" $
    eval [] (VBinary OpConcat (VString "te") (VString "st")) @?= VString "test"
  , testCase "BT I$ S4%34 -> tes" $
    eval [] (VBinary OpTake (VInt 3) (VString "test")) @?= VString "tes"
  , testCase "BD I$ S4%34 -> t" $
    eval [] (VBinary OpDrop (VInt 3) (VString "test")) @?= VString "t"
  ]

ifTests = testGroup "If eval"
  []

lambdaTests = testGroup "Lambda eval"
  []
