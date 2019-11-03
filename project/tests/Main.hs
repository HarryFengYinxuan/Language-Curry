module Main where

import Test.Tasty

import CheckTest
import EvalTest
import ParserTest

main = defaultMain testSuite


testSuite =
  testGroup
    "allTests"
    [
    EvalTest.tests,
    CheckTest.tests1,
    CheckTest.tests2,
    ParserTest.tests
    ]
