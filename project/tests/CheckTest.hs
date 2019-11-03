module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Check
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad(ap)

-- import EnvUnsafe
import qualified Data.Map as Map

-- provide tests that show your check works




eqWarning :: WarningMsg -> WarningMsg -> Bool 
eqWarning (UndefinedVarUse s1) (UndefinedVarUse s2) = s1 == s2
eqWarning (WrongType s1) (WrongType s2) = s1 == s2


tests1 = testGroup "CheckTest" 
  [
  testCase "Unbound variable error" $
      do
         assertBool "(\\x->y) has unbound var error y is not in scope" $ (check $ Lam "x" IntegerType (Var "y" IntegerType)) == (Set.singleton (UndefinedVarUse "y is not in scope"))
         assertBool "x has unbound var error x is not in scope" $ (check $ (Var "x" IntegerType)) == (Set.singleton (UndefinedVarUse "x is not in scope"))

         --assertBool "(x || y) has unbound var error x is not in scope" $ (check $ Or (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "x is not in scope"))
         --assertBool "(x || y) has unbound var error y is not in scope" $ (check $ Or (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "y is not in scope"))

         assertBool "(x || y) has unbound var error x and y are not in scope" $ (check $ Or (Var "x" BoolType) (Var "y" BoolType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])

         --assertBool "(x && y) has unbound var error x  is not in scope" $ (check $ And (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "x is not in scope"))
         --assertBool "(x && y) has unbound var error y  is not in scope" $ (check $ And (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "y is not in scope"))
         assertBool "(x && y) has unbound var error x and y are not in scope" $ (check $ And (Var "x" BoolType) (Var "y" BoolType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])
         --assertBool "(x - y) has unbound var error x is not in scope" $ (check $ Minus (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "x is not in scope"))
         --assertBool "(x - y) has unbound var error y is not in scope" $ (check $ Minus (Var "x") (Var "y")) == (Set.singleton (UndefinedVarUse "y is not in scope"))
         assertBool "(x - y) has unbound var error x and y are not in scope" $ (check $ Minus (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])
         assertBool "(x + y) has unbound var error x and y are not in scope" $ (check $ Plus (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])
         assertBool "(x * y) has unbound var error x and y are not in scope" $ (check $ Mult (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])
         assertBool "(x \\ y) has unbound var error x and y are not in scope" $ (check $ DivI (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])

         -- assertBool "app x y has unbound var error x and y are not in scope" $ (check $ App (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])
         assertBool "seprate x y has unbound var error x and y are not in scope" $ (check $ Sep (Var "x" IntegerType) (Var "y" IntegerType)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])

         assertBool "[x] has unbound var error x is not in scope" $ (check $ (Cons (Var "x" IntegerType) Nil)) == (Set.singleton (UndefinedVarUse "x is not in scope"))

         assertBool "Cons x y has unbound var error x is not in scope" $ (check $ Cons (Var "x" IntegerType) (Cons (Var "y" IntegerType) Nil)) == (Set.fromAscList [(UndefinedVarUse "x is not in scope"), (UndefinedVarUse "y is not in scope")])

         assertBool "(Print x) has unbound var error x is not in scope" $ (check $ Print (Var "x" IntegerType)) == (Set.singleton (UndefinedVarUse "x is not in scope"))
        


  ]

tests2 = testGroup "StaticCheckTest"
  [
       
  testCase "getType test" $
      do 
         assertBool "Int is int" ((Ok IntegerType) == Check.getType (ValInt 1))
         assertBool "Bool is bool" ((Ok BoolType) == Check.getType (ValBool True))
         assertBool "Lam is right" ((Ok FuncType) == Check.getType (Lam "x" IntegerType (Var "x" IntegerType)))
         assertBool "App is right" ((Ok IntegerType) == Check.getType (App (Lam "x" IntegerType (ValInt 1)) (ValInt 1))),
  testCase "Check examples" $
      do 
         assertBool ("Check "++ show ast0) $ (Set.singleton FineGram) == (check ast0)
         assertBool ("Check "++ show ast1) $ (Set.singleton FineGram) == (check ast1)
         assertBool ("Check "++ show ast2) $ (Set.singleton FineGram) == (check ast2)
         assertBool ("Check "++ show ast3) $ (Set.singleton FineGram) == (check ast3)
         assertBool ("Check "++ show ast4) $ (Set.singleton FineGram) == (check ast4)
         assertBool ("Check "++ show ast5) $ (Set.singleton FineGram) == (check ast5)
         assertBool ("Check "++ show ast6) $ (Set.singleton FineGram) == (check ast6)
         assertBool ("Check "++ show ast7) $ (Set.singleton FineGram) == (check ast7)
         assertBool ("Check "++ show ast8) $ (Set.singleton FineGram) == (check ast8)
         assertBool ("Check "++ show ast9) $ (Set.singleton FineGram) == (check ast9)
         assertBool ("Check "++ show ast10) $ (Set.singleton FineGram) == (check ast10)
         assertBool ("Check "++ show ast11) $ (Set.singleton FineGram) == (check ast11)
         assertBool ("Check "++ show ast12) $ (Set.singleton FineGram) == (check ast12)
         assertBool ("Check "++ show ast13) $ (Set.singleton FineGram) == (check ast13)
         assertBool ("Check "++ show ast14) $ (Set.singleton FineGram) == (check ast14)
         assertBool ("Check "++ show ast15) $ (Set.singleton FineGram) == (check ast15)
         assertBool ("Check "++ show ast16) $ (Set.singleton FineGram) == (check ast16)
         assertBool ("Check "++ show ast17) $ (Set.singleton FineGram) == (check ast17)
         assertBool ("Check "++ show ast18) $ (Set.singleton FineGram) == (check ast18)
         assertBool ("Check "++ show ast19) $ (Set.singleton FineGram) == (check ast19)
         assertBool ("Check "++ show ast20) $ (Set.singleton FineGram) == (check ast20)
         assertBool ("Check "++ show ast21) $ (Set.singleton FineGram) == (check ast21)
         assertBool ("Check "++ show ast22) $ (Set.singleton FineGram) == (check ast22)
         assertBool ("Check "++ show ast23) $ (Set.singleton FineGram) == (check ast23)
         assertBool ("Check "++ show ast24) $ (Set.singleton FineGram) == (check ast24)
         assertBool ("Check "++ show ast25) $ (Set.singleton FineGram) == (check ast25)
         assertBool ("Check "++ show ast26) $ (Set.singleton FineGram) == (check ast26)
         assertBool ("Check "++ show ast27) $ (Set.singleton FineGram) == (check ast27)
         assertBool ("Check "++ show ast28) $ (Set.singleton FineGram) == (check ast28)
         assertBool ("Check "++ show ast29) $ (Set.singleton FineGram) == (check ast29)
         assertBool ("Check "++ show ast30) $ (Set.singleton FineGram) == (check ast30)
         assertBool ("Check "++ show ast31) $ (Set.singleton FineGram) == (check ast31)
         
         assertBool ("Check "++ show ast32) $ (Set.singleton (UndefinedVarUse "x is not in scope")) == (check ast32)
         assertBool ("Check "++ show ast33) $ (Set.singleton (UndefinedVarUse "y is not in scope")) == (check ast33)
         assertBool ("Check "++ show ast34) $ (Set.singleton (UndefinedVarUse "b is not in scope")) == (check ast34)
         assertBool ("Check "++ show ast35) $ (Set.singleton (UndefinedVarUse "f is not in scope")) == (check ast35)
         assertBool ("Check "++ show ast36) $ (Set.singleton (UndefinedVarUse "string is not in scope")) == (check ast36)
         assertBool ("Check "++ show ast37) $ (Set.singleton (UndefinedVarUse "x is not in scope")) == (check ast37)
         assertBool ("Check "++ show ast38) $ (Set.singleton (UndefinedVarUse "y is not in scope")) == (check ast38)
         assertBool ("Check "++ show ast39) $ (Set.singleton (UndefinedVarUse "b is not in scope")) == (check ast39)
         
         assertBool ("Check "++ show ast40) $ (Set.singleton FineGram) == (check ast40)
         assertBool ("Check "++ show ast41) $ (Set.singleton FineGram) == (check ast41)
         assertBool ("Check "++ show ast42) $ (Set.singleton FineGram) == (check ast42)
         assertBool ("Check "++ show ast43) $ (Set.singleton FineGram) == (check ast43)

         assertBool ("Check "++ show error1) $ (Set.singleton (WrongType $ "Type Error for " ++ show error1)) == (check error1)
         assertBool ("Check "++ show error2) $ (Set.singleton (WrongType $ "Type Error for " ++ show error2)) == (check error2)
         assertBool ("Check "++ show error3) $ (Set.singleton (WrongType $ "Type Error for " ++ show error3)) == (check error3)
         assertBool ("Check "++ show error4) $ (Set.singleton (WrongType $ "Type Error for " ++ show error1)) == (check error4)
         assertBool ("Check "++ show error5) $ (Set.singleton (WrongType $ "Type Error for " ++ show error1)) == (check error5)

  ]
