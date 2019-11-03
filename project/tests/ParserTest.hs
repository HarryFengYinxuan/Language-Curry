module ParserTest where

  import Test.Tasty (testGroup)
  import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
  import Test.Tasty.QuickCheck (testProperty,Arbitrary, oneof,arbitrary )
  
  import Eval
  import Ast
  import Parser
  import EnvUnsafe
  import Control.Monad(ap)
  import ParserMonad
  import Data.Maybe
  import qualified Data.Map as Map
  
  -- provide tests that show your parser works
  tests = testGroup "ParserTest" 
    [
      testCase "Parser Bool" $
        do
          assertBool "True is itself" $ (myPar "true") == (Just (ValBool True, ""))
          assertBool "False is itself" $ (myPar "false") == (Just (ValBool False, ""))
          assertBool "True and False is itself" $ (myPar "true && false") == (Just (And (ValBool True) (ValBool False), ""))
          assertBool "True and True is itself" $ (myPar "true && true") == (Just (And (ValBool True) (ValBool True), ""))
          assertBool "True or False is itself" $ (myPar "true || false") == (Just (Or (ValBool True) (ValBool False), ""))
          assertBool "False or False is itself" $ (myPar "false || false") == (Just (Or (ValBool False) (ValBool False), ""))
          assertBool "not True is itself" $ (myPar "!true") == (Just (Not (ValBool True), ""))
          assertBool "not False is itself" $ (myPar "!false") == (Just (Not (ValBool False), "")),
  
      testCase "Parser Number" $ 
        do
          assertBool "1.2 is itself" $ (myPar "1.2") == (Just (ValFloat 1.2, ""))
          assertBool "3 is itself" $ (myPar "3") == (Just (ValInt 3, ""))
          assertBool "3 + 7 is itself" $ (myPar "3+7") == (Just (Plus (ValInt 3) (ValInt 7), ""))
          assertBool "10 - 7 is itself" $ (myPar "10-7") == (Just (Minus (ValInt 10) (ValInt 7), ""))
          assertBool "3 * 7 is itself" $ (myPar "3*7") == (Just (Mult (ValInt 3) (ValInt 7), ""))
          assertBool "14 // 7 is itself" $ (myPar "14//7") == (Just (DivI (ValInt 14) (ValInt 7), ""))
          assertBool "14 // 0 is itself" $ (myPar "14//0") == (Just (DivI (ValInt 14) (ValInt 0), ""))
          assertBool "14.3 / 7 is itself" $ (myPar "14.3/7") == (Just (DivF (ValFloat 14.3) (ValInt 7), ""))
          assertBool "14.3 / 0 is itself" $ (myPar "14.3/0") == (Just (DivF (ValFloat 14.3) (ValInt 0), "")),
  
  --    testCase "Parser ValChar & ValStr" $
  --      do
  --        assertBool "c is c" $ (myPar "c") == (Just (ValChar 'c',""))
  --        assertBool "genius is genius" $ (myPar "genius") == (Just (ValStr "genius", "")),
  
      testCase "Parser If & Let" $
        do
          assertBool "if True then 3 else 5" $ (myPar "if true then 3 else 5") == (Just (If (ValBool True) (ValInt 3) (ValInt 5), ""))
          assertBool "Let should match parse" $ (myPar "let x = 2 in 2") == (Just (Let "x" (ValInt 2) (ValInt 2), "")),
  
  
      testCase "Parser Var" $
        do
          assertBool "Var String" $ (myPar "genius:i") == (Just (Var "genius" IntegerType, "")),
  
      testCase "Parser Lam" $
        do assertBool "lambda x -> 2 + 3" $ (myPar "\\x:i -> 2 + 3") == (Just (Lam "x" IntegerType (Plus (ValInt 2) (ValInt 3)) , "")),
      testCase "Parser App" $
        do assertBool "(\\ x -> 2 + 3) 1" $ (myPar "(\\x:i -> 2 + 3) 1") == (Just (App (Lam "x" IntegerType (Plus (ValInt 2) (ValInt 3))) (ValInt 1), "")),
      testCase "Parser Var" $
        do assertBool "abc" $ (myPar "abc:i") == (Just ((Var "abc" IntegerType), "")),
      testCase "Parser []" $
        do assertBool "[]" $ (myPar "[]") == (Just (Nil, "")),
      testCase "Parser char" $
        do assertBool "'a'" $ (myPar "'a'") == (Just (ValChar 'a', "")),
      testCase "Parser string" $
        do assertBool "\"Hello\"" $ (myPar "\"Hello\"") == (Just (ValStr "Hello", "")),
      testCase "Parser unary, and print" $
        do 
          assertBool "unary 1" $ (myPar "-1") == (Just (UMinus (ValInt 1), ""))
          assertBool "unary 3.5" $ (myPar "-3.5") == (Just (UMinus (ValFloat 3.5), ""))
          assertBool "print" $ (myPar "print(\"Hello\")") == (Just (Print (ValStr "Hello"), ""))
          assertBool "print []" $ (myPar "print([])") == (Just (Print (Nil), "")),
      testCase "Parser list indexing" $
        do 
          assertBool "[!true, 1] !! 0" $ (myPar "[!true, 1] !! 0") == (Just (Ind (Cons (Not (ValBool True)) (Cons (ValInt 1) (Nil))) (ValInt 0),"")),
      testCase "Parser exp" $
        do 
          assertBool "1 ** 9" $ (myPar "1 ** 9") == (Just (ExpI (ValInt 1) (ValInt 9),""))
          assertBool "1.5 ^ 9.5" $ (myPar "1.5 ^ 9.5") == (Just (ExpF (ValFloat 1.5) (ValFloat 9.5),"")),
      testCase "Parser mod and integer division" $
        do 
          assertBool "2 % 3" $ (myPar "2 % 3") == (Just (Mod (ValInt 2) (ValInt 3),""))
          assertBool "2 // 3" $ (myPar "2 // 3") == (Just (DivI (ValInt 2) (ValInt 3),"")),
      testCase "Parser concat and cons" $
        do 
          assertBool "[1, 2]" $ (myPar "[1, 2]") == (Just (Cons (ValInt 1) (Cons (ValInt 2) (Nil)),""))
          assertBool "[1] ++ [2]" $ (myPar "[1] ++ [2]") == (Just (Concat (Cons (ValInt 1) Nil) (Cons (ValInt 2) Nil),"")),
      testCase "Parser comp" $
        do 
          assertBool "1 < 2" $ (myPar "1 < 2") == (Just (Ast.LT (ValInt 1) (ValInt 2),""))
          assertBool "2 <= 2" $ (myPar "2 <= 2") == (Just (Ast.LTE (ValInt 2) (ValInt 2),""))
          assertBool "1 == 2" $ (myPar "1 == 2") == (Just (Ast.EQ (ValInt 1) (ValInt 2),""))
          assertBool "2 >= 2" $ (myPar "2 >= 2") == (Just (Ast.GTE (ValInt 2) (ValInt 2),""))
          assertBool "1 /= 2" $ (myPar "1 /= 2") == (Just (Ast.NEQ (ValInt 1) (ValInt 2),""))
          assertBool "2 > 2" $ (myPar "2 > 2") == (Just (Ast.GT (ValInt 2) (ValInt 2),"")),
      -- testCase "Parser comment" $
      --   do 
      --     assertBool "{-abc-}"
      testCase "Parser sep" $
        do 
          assertBool "1;1.5 ^ 9.5" $ (myPar "1;1.5 ^ 9.5") == (Just (Sep (ValInt 1) (ExpF (ValFloat 1.5) (ValFloat 9.5)),"")),
      testCase "Parser comment" $
        do 
          assertBool "1;1.5 -- ^ 9.5\n" $ (myPar "1;1.5-- ^ 9.5\n") == (Just (Sep (ValInt 1) (ValFloat 1.5),""))
          assertBool "{-abcde-}" $ (myPar "{-abcde-}") == Nothing
          assertBool "{-abcde-}" $ (myPar "{-abcde-}1") == Just (ValInt 1, "")
          assertBool "{-abcde-}string:s" $ (myPar "{-abcde-}string:s") == Just (Var "string" StringType, ""),
      testCase "Parser error" $
        do 
          assertBool "let true"  $ (myPar "let true" ) == Nothing
          assertBool "if true then 1 else 2, 2" $ (myPar "if true then 1 else 2, 2") == Just (If (ValBool True) (ValInt 1) (ValInt 2), ", 2")
          assertBool "[wrong->wrong]" $ (myPar "[wrong->wrong]") == Nothing
          assertBool "varWithNoAnnotation" $ (myPar "varWithNoAnnotation") == Nothing,
      testCase "Parser examples" $
        do 
          assertBool (show ast0) $ (myPar (show ast0)) == Just (ast0, "")
          assertBool (show ast1) $ (myPar (show ast1)) == Just (ast1, "")
          assertBool (show ast2) $ (myPar (show ast2)) == Just (ast2, "")
          assertBool (show ast3) $ (myPar (show ast3)) == Just (ast3, "")
          assertBool (show ast4) $ (myPar (show ast4)) == Just (ast4, "")
          assertBool (show ast5) $ (myPar (show ast5)) == Just (ast5, "")
          assertBool (show ast6) $ (myPar (show ast6)) == Just (ast6, "")
          assertBool (show ast7) $ (myPar (show ast7)) == Just (ast7, "")
          assertBool (show ast8) $ (myPar (show ast8)) == Just (ast8, "")
          assertBool (show ast9) $ (myPar (show ast9)) == Just (ast9, "")
          assertBool (show ast10) $ (myPar (show ast10)) == Just (ast10, "")
          assertBool (show ast11) $ (myPar (show ast11)) == Just (ast11, "")
          assertBool (show ast12) $ (myPar (show ast12)) == Just (ast12, "")
          assertBool (show ast13) $ (myPar (show ast13)) == Just (ast13, "")
          assertBool (show ast14) $ (myPar (show ast14)) == Just (ast14, "")
          assertBool (show ast15) $ (myPar (show ast15)) == Just (ast15, "")
          assertBool (show ast16) $ (myPar (show ast16)) == Just (ast16, "")
          assertBool (show ast17) $ (myPar (show ast17)) == Just (ast17, "")
          assertBool (show ast18) $ (myPar (show ast18)) == Just (ast18, "")
          assertBool (show ast19) $ (myPar (show ast19)) == Just (ast19, "")
          assertBool (show ast20) $ (myPar (show ast20)) == Just (ast20, "")
          assertBool (show ast21) $ (myPar (show ast21)) == Just (ast21, "")
          assertBool (show ast22) $ (myPar (show ast22)) == Just (ast22, "")
          assertBool (show ast23) $ (myPar (show ast23)) == Just (ast23, "")
          assertBool (show ast24) $ (myPar (show ast24)) == Just (ast24, "")
          assertBool (show ast25) $ (myPar (show ast25)) == Just (ast25, "")
          assertBool (show ast26) $ (myPar (show ast26)) == Just (ast26, "")
          assertBool (show ast27) $ (myPar (show ast27)) == Just (ast27, "")
          assertBool (show ast28) $ (myPar (show ast28)) == Just (ast28, "")
          assertBool (show ast29) $ (myPar (show ast29)) == Just (ast29, "")
          assertBool (show ast30) $ (myPar (show ast30)) == Just (ast30, "")
          assertBool (show ast31) $ (myPar (show ast31)) == Just (ast31, "")
          assertBool (show ast32) $ (myPar (show ast32)) == Just (ast32, "")
          assertBool (show ast33) $ (myPar (show ast33)) == Just (ast33, "")
          assertBool (show ast34) $ (myPar (show ast34)) == Just (ast34, "")
          assertBool (show ast35) $ (myPar (show ast35)) == Just (ast35, "")
          assertBool (show ast36) $ (myPar (show ast36)) == Just (ast36, "")
          assertBool (show ast37) $ (myPar (show ast37)) == Just (ast37, "")
          assertBool (show ast38) $ (myPar (show ast38)) == Just (ast38, "")
          assertBool (show ast39) $ (myPar (show ast39)) == Just (ast39, "")
          assertBool (show ast40) $ (myPar (show ast40)) == Just (ast40, "")
          assertBool (show ast41) $ (myPar (show ast41)) == Just (ast41, "")
          assertBool (show ast42) $ (myPar (show ast42)) == Just (ast42, "")
          assertBool (show ast43) $ (myPar (show ast43)) == Just (ast43, "")

          assertBool (show ast44) $ (myPar (show ast44)) == Just (ast44, "")
          assertBool (show ast45) $ (myPar (show ast45)) == Just (ast45, "")
          assertBool (show ast46) $ (myPar (show ast46)) == Just (ast46, "")
          assertBool (show ast47) $ (myPar (show ast47)) == Just (ast47, "")
          assertBool (show ast48) $ (myPar (show ast48)) == Just (ast48, "")
          assertBool (show ast49) $ (myPar (show ast49)) == Just (ast49, "")
          assertBool (show ast50) $ (myPar (show ast50)) == Just (ast50, "")
          assertBool (show ast51) $ (myPar (show ast51)) == Just (ast51, "")
          assertBool (show ast52) $ (myPar (show ast52)) == Just (ast52, "")
          assertBool (show ast53) $ (myPar (show ast53)) == Just (ast53, "")
          assertBool (show ast54) $ (myPar (show ast54)) == Just (ast54, "")
          assertBool (show ast55) $ (myPar (show ast55)) == Just (ast55, "")
          assertBool (show program) $ (myPar (show program)) == Just (program, "")
  
  --    testCase "Parser App" $
  --      do assertBool "apply (lambda x -> 2 + 3) on 7" $ (myPar "\\x->2+3 7") == (Just ((App (Lam "x" (Plus (ValInt 2) (ValInt 3))) (ValInt 7)), ""))
    ]
  
  