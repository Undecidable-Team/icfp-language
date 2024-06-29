{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Parser
import Types
import Eval

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ evalTests
  , parserTests
  ]

evalTests = testGroup "Eval"
  [ strToIntTests
  , intToStrTests
  -- , s2iRoundTests
  , unaryTests
  , binaryTests
  , ifTests
  , lambdaTests
  , fullTest
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
  [ testCase "? B> I# I$ S9%3 S./" $
    eval [] (VIf (VBinary OpGT (VInt 2) (VInt 3)) (VString "yes") (VString "no")) @?= VString "no"
  ]

lambdaTests = testGroup "Lambda eval"
  [ testCase "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" $
    eval []
    (VBinary OpApp
      (VBinary OpApp
        (VLam (Name {unName = 2})
         (VLam (Name {unName = 3})
          (VVar (Name {unName = 2}))))
        (VBinary OpConcat (VString "Hello") (VString " World!")))
      (VInt 42)) @?= VString "Hello World!"
  ]

fullTest = testCase "get language_test" $ let
  e = VIf (VBinary OpEQ (VBinary OpApp (VBinary OpApp (VBinary OpApp (VBinary OpApp (VLam (Name {unName = 3}) (VLam (Name {unName = 3}) (VLam (Name {unName = 3}) (VLam (Name {unName = 2}) (VVar (Name {unName = 3})))))) (VInt 1)) (VInt 2)) (VInt 3)) (VInt 4)) (VInt 3)) (VIf (VBinary OpEQ (VBinary OpApp (VLam (Name {unName = 3}) (VVar (Name {unName = 3}))) (VInt 10)) (VInt 10)) (VIf (VBinary OpEQ (VBinary OpDrop (VInt 3) (VString "test")) (VString "t")) (VIf (VBinary OpEQ (VBinary OpTake (VInt 3) (VString "test")) (VString "tes")) (VIf (VBinary OpEQ (VBinary OpConcat (VString "te") (VString "st")) (VString "test")) (VIf (VUnary OpNot (VBinary OpAnd (VBool True) (VBool False))) (VIf (VBinary OpAnd (VBool True) (VBool True)) (VIf (VUnary OpNot (VBinary OpOr (VBool False) (VBool False))) (VIf (VBinary OpOr (VBool False) (VBool True)) (VIf (VBinary OpLT (VUnary OpNeg (VInt 3)) (VUnary OpNeg (VInt 2))) (VIf (VBinary OpGT (VInt 3) (VInt 2)) (VIf (VBinary OpEQ (VUnary OpNeg (VInt 1)) (VBinary OpMod (VUnary OpNeg (VInt 3)) (VInt 2))) (VIf (VBinary OpEQ (VInt 1) (VBinary OpMod (VInt 7) (VInt 3))) (VIf (VBinary OpEQ (VUnary OpNeg (VInt 1)) (VBinary OpDiv (VUnary OpNeg (VInt 3)) (VInt 2))) (VIf (VBinary OpEQ (VInt 2) (VBinary OpDiv (VInt 7) (VInt 3))) (VIf (VBinary OpEQ (VInt 6) (VBinary OpMul (VInt 2) (VInt 3))) (VIf (VBinary OpEQ (VInt 3) (VBinary OpAdd (VInt 1) (VInt 2))) (VIf (VBinary OpEQ (VUnary OpIntToString (VInt 15818151)) (VString "test")) (VIf (VBinary OpEQ (VUnary OpStringToInt (VString "test")) (VInt 15818151)) (VIf (VUnary OpNot (VBool False)) (VIf (VBinary OpEQ (VUnary OpNeg (VInt 3)) (VBinary OpSub (VInt 2) (VInt 5))) (VIf (VBinary OpEQ (VInt 3) (VBinary OpSub (VInt 5) (VInt 2))) (VIf (VBinary OpEQ (VString "test") (VString "test")) (VIf (VBinary OpEQ (VBool False) (VBool False)) (VIf (VBinary OpEQ (VInt 3) (VInt 3)) (VIf (VBool True) (VBinary OpConcat (VBinary OpConcat (VString "Self-check OK, send `solve language_test ") (VUnary OpIntToString (VBinary OpAdd (VInt 2) (VBinary OpMul (VInt 311) (VInt 124753942619))))) (VString "` to claim points for it")) (VString "if is not correct")) (VString "binary = is not correct")) (VString "binary = is not correct")) (VString "binary = is not correct")) (VString "binary - is not correct")) (VString "unary - is not correct")) (VString "unary ! is not correct")) (VString "unary # is not correct")) (VString "unary $ is not correct")) (VString "binary + is not correct")) (VString "binary * is not correct")) (VString "binary / is not correct")) (VString "binary / is not correct")) (VString "binary % is not correct")) (VString "binary % is not correct")) (VString "binary > is not correct")) (VString "binary < is not correct")) (VString "binary | is not correct")) (VString "binary | is not correct")) (VString "binary & is not correct")) (VString "binary & is not correct")) (VString "binary . is not correct")) (VString "binary T is not correct")) (VString "binary D is not correct")) (VString "application is not correct")) (VString "application is not correct")
  in eval [] e @?= VString "solve language_test ANSWER_HERE"

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ parseBoolTests
  , parseIntegerTests
  , parseStringTests
  , parseUnOpTests
  , parseBiOpTests
  , parseIfTests
  , parseLamTests
  , parseVarTests
  ]

parseBoolTests :: TestTree
parseBoolTests = testGroup "parseBool"
  [ testCase "spec: T" $ readWithParser parseBool "T" @?= Right (VBool True)
  , testCase "spec: F" $ readWithParser parseBool "F" @?= Right (VBool False)
  ]

parseIntegerTests :: TestTree
parseIntegerTests = testGroup "parseInteger"
  [ testCase "spec: I!" $ readWithParser parseInteger "I!" @?= Right (VInt 0)
  , testCase "spec: I\"" $ readWithParser parseInteger "I\"" @?= Right (VInt 1)
  , testCase "spec: I/6" $ readWithParser parseInteger "I/6" @?= Right (VInt 1337)
  , testCase "spec: I#" $ readWithParser parseInteger "I#" @?= Right (VInt 2)
  , testCase "spec: I$" $ readWithParser parseInteger "I$" @?= Right (VInt 3)
  ]

parseStringTests :: TestTree
parseStringTests = testGroup "parseString"
  [ testCase "spec: SB%,,/}Q/2,$_" $ readWithParser parseString "SB%,,/}Q/2,$_" @?= Right (VString "Hello World!")
  ]

parseUnOpTests :: TestTree
parseUnOpTests = testGroup "parseUnOp"
  [ testCase "spec: U- I$" $ readWithParser parseUnOp "U- I$" @?= Right (VUnary OpNeg (VInt 3))
  , testCase "spec: U! T" $ readWithParser parseUnOp "U! T" @?= Right (VUnary OpNot (VBool True))
  , testCase "spec: U# S4%34" $ readWithParser parseUnOp "U# S4%34" @?= Right (VUnary OpStringToInt (VString "test"))
  , testCase "spec: U$ I4%34" $ readWithParser parseUnOp "U$ I4%34" @?= Right (VUnary OpIntToString (VInt 15818151))
  ]

parseBiOpTests :: TestTree
parseBiOpTests = testGroup "parseBiOp"
  [ testCase "spec: B+ I# I$" $ readWithParser parseBiOp "B+ I# I$" @?= Right (VBinary OpAdd (VInt 2) (VInt 3))
  , testCase "spec: B- I$ I#" $ readWithParser parseBiOp "B- I$ I#" @?= Right (VBinary OpSub (VInt 3) (VInt 2))
  , testCase "spec: B* I$ I#" $ readWithParser parseBiOp "B* I$ I#" @?= Right (VBinary OpMul (VInt 3) (VInt 2))
  , testCase "spec: B/ U- I( I#" $ readWithParser parseBiOp "B/ U- I( I#" @?= Right (VBinary OpDiv (VUnary OpNeg (VInt 7)) (VInt 2))
  , testCase "spec: B% U- I( I#" $ readWithParser parseBiOp "B% U- I( I#" @?= Right (VBinary OpMod (VUnary OpNeg (VInt 7)) (VInt 2))
  , testCase "spec: B< I$ I#" $ readWithParser parseBiOp "B< I$ I#" @?= Right (VBinary OpLT (VInt 3) (VInt 2))
  , testCase "spec: B> I$ I#" $ readWithParser parseBiOp "B> I$ I#" @?= Right (VBinary OpGT (VInt 3) (VInt 2))
  , testCase "spec: B= I$ I#" $ readWithParser parseBiOp "B= I$ I#" @?= Right (VBinary OpEQ (VInt 3) (VInt 2))
  , testCase "spec: B| T F" $ readWithParser parseBiOp "B| T F" @?= Right (VBinary OpOr (VBool True) (VBool False))
  , testCase "spec: B& T F" $ readWithParser parseBiOp "B& T F" @?= Right (VBinary OpAnd (VBool True) (VBool False))
  , testCase "spec: B. S4% S34" $ readWithParser parseBiOp "B. S4% S34" @?= Right (VBinary OpConcat (VString "te") (VString "st"))
  , testCase "spec: BT I$ S4%34" $ readWithParser parseBiOp "BT I$ S4%34" @?= Right (VBinary OpTake (VInt 3) (VString "test"))
  , testCase "spec: BD I$ S4%34" $ readWithParser parseBiOp "BD I$ S4%34" @?= Right (VBinary OpDrop (VInt 3) (VString "test"))
  ]

parseIfTests :: TestTree
parseIfTests = testGroup "parseIf"
  [ testCase "spec: ? B> I# I$ S9%3 S./" $
      readWithParser parseIf "? B> I# I$ S9%3 S./" @?= Right (VIf (VBinary OpGT (VInt 2) (VInt 3)) (VString "yes") (VString "no"))
  ]

parseLamTests :: TestTree
parseLamTests = testGroup "parseLam"
  [ testCase "spec: B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" $
      readWithParser parseBiOp "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"
        @?= Right (VBinary OpApp (VBinary OpApp (VLam (Name 2) (VLam (Name 3) (VVar (Name 2)))) (VBinary OpConcat (VString "Hello") (VString " World!"))) (VInt 42))
  ]

parseVarTests :: TestTree
parseVarTests = testGroup "parseVar"
  [ testCase "spec: v#" $ readWithParser parseVar "v#" @?= Right (VVar (Name 2))
  ]
