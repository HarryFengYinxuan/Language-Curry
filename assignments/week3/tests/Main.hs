<<<<<<< HEAD
module Main where


import IntTests (intTests)
import ListTests (listTests)
import OrderingTests (orderingTests)
import SetTests (setTests)
import StudentTests (studentTests)
import System.Environment

import Hw02
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  
main = 
    do 
        setEnv "TASTY_TIMEOUT" "5s"
        setEnv "TASTY_QUICKCHECK_TESTS" "20"
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain allTests
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

allTests = testGroup "all tests" [
        intTests,
        listTests,
        orderingTests,
        setTests,
        studentTests
    ]

=======
module Main where


import IntTests (intTests)
import ListTests (listTests)
import OrderingTests (orderingTests)
import SetTests (setTests)
import StudentTests (studentTests)
import System.Environment

import Hw02
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )

  
main = 
    do 
        setEnv "TASTY_TIMEOUT" "5s"
        setEnv "TASTY_QUICKCHECK_TESTS" "20"
        setEnv "TASTY_QUICKCHECK_MAX_SIZE" "50"
        defaultMain allTests
        unsetEnv "TASTY_TIMEOUT"
        unsetEnv "TASTY_QUICKCHECK_TESTS"
        unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"

allTests = testGroup "all tests" [
        intTests,
        listTests,
        orderingTests,
        setTests,
        studentTests
    ]

>>>>>>> 309081b7613d989c553e5e45e5fb838442f4b773
