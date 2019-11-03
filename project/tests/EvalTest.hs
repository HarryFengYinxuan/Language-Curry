module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval
import EnvUnsafe
import qualified Data.Map as Map

-- provide tests that show your run/eval works

-- | Test if two Unsafe Boolean is equal 
eqBool :: Bool -> Unsafe Val -> Bool 
eqBool b1 (Ok (B b2)) = b1 == b2 
eqBool _ _ = False

eqFloat :: Float -> Unsafe Val -> Bool 
eqFloat f1 (Ok (F f2)) = f1 == f2 
eqFloat _ _ = False

eqChar :: Char -> Unsafe Val -> Bool 
eqChar c1 (Ok (C c2)) = c1 == c2 
eqChar _ _ = False 

eqStr :: String -> Unsafe Val -> Bool 
eqStr s1 (Ok (S s2)) = s1 == s2 
eqStr _ _ = False

eqInt :: Integer -> Unsafe Val -> Bool 
eqInt i1 (Ok (I i2)) = i1 == i2 
eqInt _ _ = False

-- | test if an unsafe is error
isError :: Unsafe a -> Bool 
isError (Error _) = True 
isError _ = False 

-- declear cons as a right associative infix 
infixr 7 `cons`
cons = Cons

-- | run a program under empty exprssion
runEmpty :: Ast -> Unsafe Val 
runEmpty exp = let (a, b) = runEnvUnsafeLog (eval exp) Map.empty in a
runEmptyT :: Ast -> Unsafe TestVal 
runEmptyT exp = 
  case runEnvUnsafeLog (eval exp) Map.empty of 
    (Ok res, _) -> Ok $ valToTestVal res 
    (Error msg, _) -> Error msg
runEmptyTL :: Ast -> (Unsafe TestVal, [String])
runEmptyTL exp = 
  case runEnvUnsafeLog (eval exp) Map.empty of 
    (Ok res, s) -> (Ok $ valToTestVal res, s)
    (Error msg, s) -> (Error msg, s)

-- | an unsafe function to convert Val to TestVal
valToTestVal :: Val -> TestVal 
valToTestVal (I n) = TI n 
valToTestVal (B b) = TB b 
valToTestVal (F n) = TF n 
valToTestVal (S b) = TS b 
valToTestVal (Ls lst) = TLs $ map valToTestVal lst 
valToTestVal (Eval.Fun _) = error "unexpected function construction"

data TestVal = TI Integer | TB Bool | TF Float | TS String | TC Char
  | TLs [TestVal] deriving (Eq, Show)


tests = testGroup "EvalTest" 
  [
    testCase "Eval bool" $ 
      do
        assertBool "True is True" $ True `eqBool` (runJV (ValBool True))
        assertBool "And is correct" $ (True && True) `eqBool` (runJV (And (ValBool True) (ValBool True)))
        assertBool "And is correct 2" $ (True && False) `eqBool` (runJV (And (ValBool True) (ValBool False)))
        assertBool "Or is correct" $ (True || True) `eqBool` (runJV (Or (ValBool True) (ValBool True)))
        assertBool "Or is correct 2" $ (True || False) `eqBool` (runJV (Or (ValBool True) (ValBool False)))
        assertBool "Not is correct" $ (not True) `eqBool` (runJV (Not (ValBool True)))
        assertBool "Not is correct 2" $ (not False) `eqBool` (runJV (Not (ValBool False))),
    testCase "Eval char" $
      do 
        assertBool "Char is itself" $ 'a' `eqChar` (runJV (ValChar 'a')),
    testCase "Eval string" $
      do 
        assertBool "String is itself" $ "abc" `eqStr` (runJV (ValStr "abc")),
    testCase "Eval float" $
      do
        assertBool "Float is itself" $ 1.3 `eqFloat` (runJV (ValFloat 1.3))
        assertBool "Float is itself" $ 2.13 `eqFloat` (runJV (ValFloat 2.13))
        assertBool "Float correctly plus" $ (1.3 + 2.13) `eqFloat` (runJV (Plus (ValFloat 1.3) (ValFloat 2.13)))
        assertBool "Float correctly plus 2" $ (1.3 + 5.13) `eqFloat` (runJV (Plus (ValFloat 1.3) (ValFloat 5.13)))
        assertBool "Float correctly minus" $ (1.3 - 2.13) `eqFloat` (runJV (Minus (ValFloat 1.3) (ValFloat 2.13)))
        assertBool "Float correctly minus 2" $ (1.3 - 5.13) `eqFloat` (runJV (Minus (ValFloat 1.3) (ValFloat 5.13)))
        assertBool "Float correctly multiply" $ (1.3 * 2.13) `eqFloat` (runJV (Mult (ValFloat 1.3) (ValFloat 2.13)))
        assertBool "Float correctly multiply 2" $ (1.3 * 5.13) `eqFloat` (runJV (Mult (ValFloat 1.3) (ValFloat 5.13)))
        assertBool "Float correctly divide" $ (1.3 / 2.13) `eqFloat` (runJV (DivF (ValFloat 1.3) (ValFloat 2.13)))
        assertBool "Float correctly divide 2" $ isError (runJV (DivF (ValFloat 1.3) (ValFloat 0.0)))
        assertBool "Float returns error doing mod" $ isError (runJV (Mod (ValFloat 1.5) (ValFloat 3)))
        assertBool "Float correctly does exp" $ (2.5 ** 3.5) `eqFloat` (runJV (ExpF (ValFloat 2.5) (ValFloat 3.5)))
        assertBool "Float correctly does unary minus" $ (-3.5) `eqFloat` (runJV (UMinus (ValFloat 3.5))),
    testCase "Eval int" $
      do
        assertBool "Int is itself" $ 1 `eqInt` (runJV (ValInt 1))
        assertBool "Int is itself" $ 2 `eqInt` (runJV (ValInt 2))
        assertBool "Int correctly plus" $ (1 + 2) `eqInt` (runJV (Plus (ValInt 1) (ValInt 2)))
        assertBool "Int correctly plus 2" $ (1 + 5) `eqInt` (runJV (Plus (ValInt 1) (ValInt 5)))
        assertBool "Int correctly minus" $ (1 - 2) `eqInt` (runJV (Minus (ValInt 1) (ValInt 2)))
        assertBool "Int correctly minus 2" $ (1 - 5) `eqInt` (runJV (Minus (ValInt 1) (ValInt 5)))
        assertBool "Int correctly multiply" $ (1 * 2) `eqInt` (runJV (Mult (ValInt 1) (ValInt 2)))
        assertBool "Int correctly multiply 2" $ (1 * 5) `eqInt` (runJV (Mult (ValInt 1) (ValInt 5)))
        assertBool "Int correctly divide" $ (1 `div` 2) `eqInt` (runJV (DivI (ValInt 1) (ValInt 2)))
        assertBool "Int correctly divide 2" $ isError (runJV (DivI (ValInt 1) (ValInt 0)))
        assertBool "Int correctly does mod" $ (2 `mod` 3) `eqInt` (runJV (Mod (ValInt 2) (ValInt 3)))
        assertBool "Int correctly does exp" $ (2 ^ 3) `eqInt` (runJV (ExpI (ValInt 2) (ValInt 3)))
        assertBool "Int correctly does unary minus" $ (-3) `eqInt` (runJV (UMinus (ValInt 3))),
    testCase "Evaluation should support multityped list" $ 
      do 
        assertEqual "[1, 2, True]" (Ok $ TLs [TI 1, TI 2, TB True]) 
          (runEmptyT $ ValInt 1 `cons` ValInt 2 `cons` ValBool True `cons` Nil)
        assertEqual "[1, True, [False, 2]]" (Ok $ TLs [TI 1, TB True, TLs [TB False, TI 2]]) 
          (runEmptyT $ ValInt 1 `cons` ValBool True `cons` (ValBool False `cons` ValInt 2 `cons` Nil) `Cons` Nil)
        assertBool "[0,1,2,3] !! 0" $ 0 `eqInt` (runJV (Ind (Cons (ValInt 0) (Cons (ValInt 1) (Cons (ValInt 2) (Cons (ValInt 3) (Nil))))) (ValInt 0)))
        assertBool "[0,1,2.5,5,3] !! 2" $ 2.5 `eqFloat` (runJV (Ind (Cons (ValInt 0) (Cons (ValInt 1) (Cons (ValFloat 2.5) (Cons (ValInt 5) (Cons (ValInt 3) (Nil)))))) (ValInt 2)))
        assertBool "[0,1,2,'h'] !! 3" $ 'h' `eqChar` (runJV (Ind (Cons (ValInt 0) (Cons (ValInt 1) (Cons (ValInt 2) (Cons (ValChar 'h') (Nil))))) (ValInt 3))),
    testCase "Eval If" $
      do 
        assertEqual "if True then 1 else 2" (Ok $ TI 1)
          (runEmptyT $ If (ValBool True) (ValInt 1) (ValInt 2)),
    testCase "Eval Let" $
      do
        assertEqual "let a = 1 in a + 1" (Ok $ TI 2)
          (runEmptyT $ Let "a" (ValInt 1) (Plus (Var "a" IntegerType) (ValInt 1))),
    testCase "Eval Var" $
      do 
        assertBool "var a" $ isError (runJV (Var "a" IntegerType)),
    testCase "Eval Lam and App" $
      do 
        assertEqual "(\\x -> x) 1" (Ok $ TI 1)
          (runEmptyT $ App (Lam "a" IntegerType (Var "a" IntegerType)) (ValInt 1))
        assertBool "(\\x -> x) 1" $ isError (runJV $ App (Lam "a" IntegerType (Var "b" IntegerType)) (ValInt 1)),
    testCase "Eval Print and Sep" $
      do 
        assertEqual "print 1; print 2" (Ok $ TI 2, ["1", "2"])
          (runEmptyTL $ Sep (Print $ ValInt 1) (Print $ ValInt 2))
        assertEqual "print \"hello\"; print \"world\"" (Ok $ TS "world", ["hello", "world"])
          (runEmptyTL $ Sep (Print $ ValStr "hello") (Print $ ValStr "world"))
        assertEqual "print \"hello\"; print \"world\"; 1+1" (Ok $ TI 2, ["hello", "world"])
          (runEmptyTL $ Sep (Print $ ValStr "hello") (Sep (Print $ ValStr "world") (Plus (ValInt 1) (ValInt 1)))),
    testCase "Eval eq and not eq" $
      do
        assertBool "1 == 1" $ True `eqBool` (runJV $ Ast.EQ (ValInt 1) (ValInt 1))
        assertBool "1 \\= 2" $ True `eqBool` (runJV $ Ast.NEQ (ValInt 1) (ValInt 2))
        assertBool "function compare is error" $ isError $ runJV $ Ast.NEQ (Lam "x" IntegerType $ ValInt 1) (ValInt 2)
        assertBool "function compare is error 2" $ isError $ runJV $ Ast.EQ (Lam "x" IntegerType $ ValInt 1) (ValInt 2)
        assertBool "1.2 == 1.2" $ True `eqBool` (runJV $ Ast.EQ (ValFloat 1.2) (ValFloat 1.2))
        assertBool "[1, 2] == [1, 2]" $ True `eqBool` (runJV $ Ast.EQ (Cons (ValInt 1) (Cons (ValInt 2) Nil)) (Cons (ValInt 1) (Cons (ValInt 2) Nil)))
        assertBool "[2, func] == [1, 2]" $ False `eqBool` (runJV $ Ast.EQ (Cons (ValInt 2) (Cons (Lam "x" IntegerType (ValInt 1)) Nil)) (Cons (ValInt 1) (Cons (ValInt 2) Nil))),
    testCase "Eval greater than and less than" $
      do 
        assertBool "[1.2, 0.9] > [1.3, 0.7]" $ False `eqBool` (runJV $ Ast.GT (Cons (ValFloat 1.2) (Cons (ValFloat 0.9) Nil)) (Cons (ValFloat 1.3) (Cons (ValFloat 0.7) Nil)))
        assertBool "1 > 1" $ False `eqBool` (runJV $ Ast.GT (ValInt 1) (ValInt 1))
        assertBool "1 > 0" $ True `eqBool` (runJV $ Ast.GT (ValInt 1) (ValInt 0))
        assertBool "Func > _" $ isError $ runJV $ Ast.GT (Lam "x" IntegerType $ ValInt 1) (ValInt 1)
        assertBool "[1.2, 0.9] < [1.3, 0.7]" $ True `eqBool` (runJV $ Ast.LT (Cons (ValFloat 1.2) (Cons (ValFloat 0.9) Nil)) (Cons (ValFloat 1.3) (Cons (ValFloat 0.7) Nil)))
        assertBool "1 < 1" $ False `eqBool` (runJV $ Ast.LT (ValInt 1) (ValInt 1))
        assertBool "1 < 0" $ False `eqBool` (runJV $ Ast.LT (ValInt 1) (ValInt 0))
        assertBool "Func < _" $ isError $ runJV $ Ast.GT (Lam "x" IntegerType $ ValInt 1) (ValInt 1),
    testCase "Eval greater than or eq and less than or eq" $
      do 
        assertBool "[1.2, 0.9] >= [1.3, 0.7]" $ False `eqBool` (runJV $ Ast.GTE (Cons (ValFloat 1.2) (Cons (ValFloat 0.9) Nil)) (Cons (ValFloat 1.3) (Cons (ValFloat 0.7) Nil)))
        assertBool "1 >= 1" $ True `eqBool` (runJV $ Ast.GTE (ValInt 1) (ValInt 1))
        assertBool "1 >= 0" $ True `eqBool` (runJV $ Ast.GTE (ValInt 1) (ValInt 0))
        assertBool "Func > _" $ isError $ runJV $ Ast.GTE (Lam "x" IntegerType $ ValInt 1) (ValInt 1)
        assertBool "[1.2, 0.9] <= [1.3, 0.7]" $ True `eqBool` (runJV $ Ast.LTE (Cons (ValFloat 1.2) (Cons (ValFloat 0.9) Nil)) (Cons (ValFloat 1.3) (Cons (ValFloat 0.7) Nil)))
        assertBool "1 <= 1" $ True `eqBool` (runJV $ Ast.LTE (ValInt 1) (ValInt 1))
        assertBool "1 <= 0" $ False `eqBool` (runJV $ Ast.LTE (ValInt 1) (ValInt 0))
        assertBool "Func <= _" $ isError $ runJV $ Ast.LTE (Lam "x" IntegerType $ ValInt 1) (ValInt 1),
    testCase ("stdLib examples") $
      do 
        assertBool (show std0) $ std0 == (Ok val0)
        assertBool (show std1) $ std1 == (Error "can only call head on a non empty list")
        assertBool (show std2) $ std2 == (Ok (Ls [val3, val4]))
        assertBool (show std3) $ std3 == (Ok (B True))
        assertBool (show std4) $ std4 == (Ok (Ls [val0]))
        assertBool (show std5) $ std5 == (Ok (I 104))
        assertBool (show std6) $ std6 == (Ok (C '\ETX'))
        assertBool (show std7) $ std7 == (Ok (F 5.0))
        assertBool (show std8) $ std8 == (Ok (I 10))
        assertBool (show std9) $ std9 == (Error "Float takes integer")
        assertBool (show std10) $ std10 == (Error "Int takes float"),
    testCase "Eval exmaples" $
        do
          assertBool (show ast0) $ ((Ok $ B True), []) == run ast0
          -- assertBool (show ast0) $ ((Ok $ TYPE VALUE), []) == run ast0
          assertBool (show ast1) $ ((Ok $ B False), []) == run ast1
          assertBool (show ast2) $ ((Ok $ B False), []) == run ast2
          assertBool (show ast3) $ ((Ok $ B False), []) == run ast3
          assertBool (show ast4) $ ((Ok $ B False), []) == run ast4
          assertBool (show ast5) $ ((Ok $ B True), []) == run ast5

          assertBool (show ast6) $ ((Ok $ I 3), []) == run ast6
          assertBool (show ast7) $ ((Ok $ I 5), []) == run ast7
          assertBool (show ast8) $ ((Ok $ I 8), []) == run ast8
          assertBool (show ast9) $ ((Ok $ I 5), []) == run ast9
          assertBool (show ast10) $ ((Ok $ I 0), []) == run ast10
          assertBool (show ast11) $ ((Ok $ I 40), []) == run ast11
          assertBool (show ast12) $ ((Ok $ I 2), []) == run ast12
          assertBool (show ast13) $ ((Error $ "can't divided by zero"), []) == run ast13

          assertBool (show ast14) $ ((Ok $ F 3.14), []) == run ast14
          assertBool (show ast15) $ ((Ok $ F 9.99), []) == run ast15
          assertBool (show ast16) $ ((Ok $ F (40*3.14)), []) == run ast16
          assertBool (show ast17) $ ((Ok $ F ((40*3.14)/9.99)), []) == run ast17
          assertBool (show ast18) $ ((Ok $ I 0), []) == run ast18
          assertBool (show ast19) $ ((Ok $ I (2^40)), []) == run ast19
          assertBool (show ast20) $ ((Ok $ F ((40*3.14)**((40*3.14)/9.99))), []) == run ast20

          assertBool (show ast21) $ ((Ok $ C 'h'), []) == run ast21
          assertBool (show ast22) $ ((Ok $ S "HelL0"), []) == run ast22
          assertBool (show ast23) $ ((Ok $ Ls []), []) == run ast23
          assertBool (show ast24) $ ((Ok $ Ls [F ((40*3.14)**((40*3.14)/9.99))]), []) == run ast24
          assertBool (show ast25) $ ((Ok $ Ls [F ((40*3.14)/9.99)]), []) == run ast25
          assertBool (show ast26) $ ((Ok $ Ls [B True, B False]), []) == run ast26
          assertBool (show ast27) $ ((Ok $ Ls [B False, B True, B False]), []) == run ast27
          assertBool (show ast28) $ ((Ok $ Ls [F ((40*3.14)**((40*3.14)/9.99)), F ((40*3.14)/9.99)]), []) == run ast28
          assertBool (show ast29) $ ((Ok $ Ls [B True, B False, B False, B True, B False]), []) == run ast29
          assertBool (show ast30) $ ((Ok $ F ((40*3.14)/9.99)), []) == run ast30
          assertBool (show ast31) $ ((Error $ "can't divided by zero"), []) == run ast31
          -- assertBool (show ast32) $ ((Ok $ TYPE VALUE), []) == run ast32
          -- assertBool (show ast33) $ ((Ok $ TYPE VALUE), []) == run ast33
          -- assertBool (show ast34) $ ((Ok $ TYPE VALUE), []) == run ast34
          -- assertBool (show ast35) $ ((Ok $ TYPE VALUE), []) == run ast35
          -- assertBool (show ast36) $ ((Ok $ TYPE VALUE), []) == run ast36
          -- assertBool (show ast37) $ ((Ok $ TYPE VALUE), []) == run ast37
          -- assertBool (show ast38) $ ((Ok $ TYPE VALUE), []) == run ast38
          -- assertBool (show ast39) $ ((Ok $ TYPE VALUE), []) == run ast39
          -- assertBool (show ast40) $ ((Ok $ TYPE VALUE), []) == run ast40
          -- assertBool (show ast41) $ ((Ok $ TYPE VALUE), []) == run ast41
          assertBool (show ast42) $ ((Ok $ I 2), []) == run ast42
          assertBool (show ast43) $ ((Ok $ F (9.99-2^40)), []) == run ast43

          assertBool (show ast44) $ ((Ok $ I 2), []) == run ast44
          assertBool (show ast45) $ ((Ok $ I 2), []) == run (App ast45 ast10)
          assertBool (show ast46) $ ((Ok $ I 2), []) == run ast46
          assertBool (show ast47) $ ((Ok $ B False), []) == run ast47
          assertBool (show ast48) $ ((Ok $ I 2), ["2"]) == run ast48
          assertBool (show ast49) $ ((Ok $ B False), ["false"]) == run ast49
          assertBool (show ast50) $ ((Ok $ B True), []) == run ast50
          assertBool (show ast51) $ ((Ok $ B True), []) == run ast51
          assertBool (show ast52) $ ((Ok $ B True), []) == run ast52
          assertBool (show ast53) $ ((Ok $ B False), []) == run ast53
          assertBool (show ast54) $ ((Ok $ B True), []) == run ast54
          assertBool (show ast55) $ ((Ok $ B True), []) == run ast55
          assertBool (show program) $ (((Ok $ S "Hi",["3.14","Hello","Hi","Hi"]))) == run program

  ]

