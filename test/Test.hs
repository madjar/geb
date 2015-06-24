module Main where

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "test test"
  [ testCase "stupid test case" $
      2 @?= 2
  ]
