module Ast where

import EnvUnsafe
import Data.Map (Map)
import qualified Data.Map as Map
import HelpShow

data Type = IntegerType | FloatType | BoolType | CharType | StringType | ListType | Void | FuncType deriving (Eq, Ord)



typeTableList = [("i", IntegerType), ("f", FloatType), ("b", BoolType), ("c", CharType), ("s", StringType), ("l", ListType), ("v", Void), ("x", FuncType)]

typeTable = Map.fromList typeTableList

typeTableInvert = Map.fromList listInvert where 
    listInvert = [(v, k)| (k, v) <- typeTableList]

-- | the abstract syntax tree for the language
-- data Ast -- ...
--         deriving (Eq,Show)  -- helpful to use this during testing
--         deriving Eq 

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | Print Ast 

         | ValInt Integer | ValFloat Float | UMinus Ast
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | DivI Ast Ast | DivF Ast Ast
         | Mod Ast Ast | ExpI Ast Ast | ExpF Ast Ast
         
         | ValChar Char | ValStr String

         | Ind Ast Ast

         | Nil
         | Cons Ast Ast
         | Concat Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String Type
         | Lam String Type Ast
         | App Ast Ast

         | Sep Ast Ast

         | NEQ Ast Ast | EQ Ast Ast 
         | LT Ast Ast | LTE Ast Ast | GT Ast Ast | GTE Ast Ast
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0

instance Show Type where 
  show t = typeTableInvert Map.! t
  

listIndex :: Ast -> Integer -> Ast 
listIndex (Nil) _ = Var "Empty" StringType
listIndex (Cons a b) i = case (i == 0) of 
                            (True) -> a 
                            (False) -> listIndex b (i-1)
listIndex _ i = Var "Wrong with listIndex in Ast" StringType


-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (ValChar c) = "(" ++ show c ++ ")"
showFullyParen (Print a) = "Print(" ++ showFullyParen a ++ ")"
showFullyParen (ValFloat i) = "(" ++ show i ++ ")"
showFullyParen (ValStr i) = "(" ++ show i ++ ")"
showFullyParen (UMinus a) = "(-" ++ showFullyParen a ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ "!" ++ (showFullyParen a) ++ ")"
showFullyParen (NEQ l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (Ast.EQ l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (Ast.LT l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (Ast.LTE l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (Ast.GT l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (Ast.GTE l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (DivF l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (DivI l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (Ind l r) = "(" ++ (showFullyParen l) ++ " !! " ++ (showFullyParen r) ++ ")"
showFullyParen (Concat l r) = "(" ++ (showFullyParen l) ++ " ++ " ++ (showFullyParen r) ++ ")"
showFullyParen (Sep l r) = "(" ++ (showFullyParen l) ++ "; " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (ExpF l r) = "(" ++ (showFullyParen l) ++ " ^ " ++ (showFullyParen r) ++ ")"
showFullyParen (ExpI l r) = "(" ++ (showFullyParen l) ++ " ** " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v t1 bod) = "(\\" ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s t) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ = if i < 0
                          then  "(" ++ show i ++ ")"
                          else show i
showPretty (ValFloat f) _ = show f
showPretty (ValChar c) _ = show c 
showPretty (ValStr s) _ = show s
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s t) _ = s ++ " :" ++ show t

showPretty (Lam v t1 bod) i = parenthesize 1 i $ "\\" ++ v ++ " :" ++ show t1 ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (Sep l r) i = parenthesize 2 i $ (showPretty l 3) ++ "; " ++ (showPretty r 2)
showPretty (App l r) i = parenthesize 3 i $ (showPretty l 3) ++ " " ++ (showPretty r 4)
-- showPretty (ValList []) i = "[]"
-- showPretty (ValList (l:r)) i = parenthesize 4 i $ "[" ++ showPretty l 5 ++ Ast.showList r ++ "]"
showPretty (Or l r) i = parenthesize 4 i $ (showPretty l 4) ++ " || " ++ (showPretty r 5)
showPretty (And l r) i = parenthesize 5 i $ (showPretty l 5) ++ " && " ++ (showPretty r 6)
showPretty (Ast.GT l r) i = parenthesize 6 i $ (showPretty l 6) ++ " > " ++ (showPretty r 7)
showPretty (Ast.GTE l r) i = parenthesize 6 i $ (showPretty l 6) ++ " >= " ++ (showPretty r 7)
showPretty (Ast.LT l r) i = parenthesize 6 i $ (showPretty l 6) ++ " < " ++ (showPretty r 7)
showPretty (Ast.LTE l r) i = parenthesize 6 i $ (showPretty l 6) ++ " <= " ++ (showPretty r 7)
showPretty (Ast.EQ l r) i = parenthesize 6 i $ (showPretty l 6) ++ " == " ++ (showPretty r 7)
showPretty (Ast.NEQ l r) i = parenthesize 6 i $ (showPretty l 6) ++ " /= " ++ (showPretty r 7)

showPretty (Cons l r) i = parenthesize 7 i $ "[" ++ showPretty l 8 ++ Ast.showList r ++ "]"
showPretty (Concat l r) i = parenthesize 7 i $ (showPretty l 8) ++ " ++ " ++ (showPretty r 7)

showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)

showPretty (Mult l r) i = parenthesize 11 i $ (showPretty l 11) ++ " * " ++ (showPretty r 12)
showPretty (DivF l r) i = parenthesize 11 i $ (showPretty l 11) ++ " / " ++ (showPretty r 12)
showPretty (DivI l r) i = parenthesize 11 i $ (showPretty l 11) ++ " // " ++ (showPretty r 12)
showPretty (Mod l r) i = parenthesize 11 i $ (showPretty l 11) ++ " % " ++ (showPretty r 12)

showPretty (ExpF l r) i = parenthesize 12 i $ (showPretty l 13) ++ " ^ " ++ (showPretty r 12)
showPretty (ExpI l r) i = parenthesize 12 i $ (showPretty l 13) ++ " ** " ++ (showPretty r 12)

showPretty (Ind l r) i = parenthesize 13 i $ (showPretty l 13) ++ "!!" ++ (showPretty r 14)
showPretty (Print l) i = parenthesize 14 i $ "print(" ++ (showPretty l 14) ++ ")"
showPretty (UMinus l) i =  "(-" ++ (showPretty l 14) ++ ")"
showPretty (Not l ) i = parenthesize 14 i $  "!" ++ (showPretty l 14)

showList :: Ast -> String 
showList (Cons h Nil) = ", " ++ showPretty h 5 
showList (Cons l r) = ", " ++ (showPretty l 5) ++ Ast.showList r
showList Nil = ""
showList _ = ""


-- bools
ast0 = ValBool True 
ast1 = ValBool False
ast2 = And ast0 ast1 -- false
ast3 = And ast1 ast2 -- false
ast4 = Not ast0 -- false
ast5 = Or ast0 ast4 -- true

-- numbers
ast6 = ValInt 3
ast7 = ValInt 5
ast8 = Plus ast6 ast7 -- 8
ast9 = Minus ast8 ast6 -- 5
ast10 = ValInt 0 
ast11 = Mult ast8 ast9 -- 40
ast12 = DivI ast8 ast6 -- 2
ast13 = DivI ast12 ast10 -- divide by zero

ast14 = ValFloat 3.14 
ast15 = ValFloat 9.99
ast16 = Mult ast11 ast14 -- (40*3.14)
ast17 = DivF ast16 ast15 -- ((40*3.14)/9.99)
ast18 = Mod ast11 ast12 -- 0
ast19 = ExpI ast12 ast11 -- (2^40)
ast20 = ExpF ast16 ast17 -- ((40*3.14)**((40*3.14)/9.99))

-- char and string
ast21 = ValChar 'h'
ast22 = ValStr "HelL0"

-- lists
ast23 = Nil -- []
ast24 = Cons ast20 ast23 -- [F ((40*3.14)**((40*3.14)/9.99))]
ast25 = Cons ast17 ast23 -- [F ((40*3.14)/9.99)]
ast26 = Cons ast5 (Cons ast4 ast23) -- [B True, B False]
ast27 = Cons ast3 ast26 -- [B False, B True, B False]
ast28 = Concat ast24 ast25 -- [F ((40*3.14)**((40*3.14)/9.99)), F ((40*3.14)/9.99)]
ast29 = Concat ast26 ast27 -- [B True, B False, B False, B True, B False]

-- if and let and var and lam
ast30 = If ast5 ast17 ast20 -- ((40*3.14)/9.99)
ast31 = If ast4 ast12 ast13 -- "can't divided by zero"

ast32 = Var "x" IntegerType
ast33 = Var "y" FloatType
ast34 = Var "b" BoolType
ast35 = Var "f" FuncType
ast36 = Var "string" StringType

ast37 = Plus ast12 ast32 
ast38 = Minus ast33 ast19 
ast39 = And ast5 ast34 

ast40 = Lam "x" IntegerType ast37 
ast41 = Lam "f" FuncType ast35

ast42 = Let "x" ast12 ast32 -- 2
ast43 = Let "y" ast15 ast38 -- (9.99-2^40)

-- app
ast44 = App ast40 ast10 -- 2
ast45 = App ast41 ast40 -- special test

-- sep and print
ast46 = Sep ast29 ast42 -- 2
ast47 = Sep ast15 ast2 -- False
ast48 = Print ast42 
ast49 = Print ast47 

-- comp
ast50 = Ast.EQ ast8 ast8 -- true
ast51 = Ast.NEQ ast8 ast15 -- true
ast52 = Ast.GT ast16 ast17 -- true
ast53 = Ast.GTE ast16 ast19 -- false
ast54 = Ast.LT ast17 ast16 -- true
ast55 = Ast.LTE ast17 ast16 -- true

program = 
  Let "x" (ValStr "Hello") (
  Let "y" (ValStr "Hi") (
  Let "pi" (ValFloat 3.14) (
  (Print (Var "pi" FloatType)) `Sep`
  Let "piSquared" ((Var "pi" FloatType) `ExpF` (ValFloat 2.0)) (
    If ((Var "piSquared" FloatType) `Ast.GT` (ValInt 10)) (
      (Print (Var "x" StringType))
    ) (
      (Print (Var "y" StringType))
    )
  )
  )
  )
  )

error1 = Plus ast2 ast14 
error2 = And ast2 ast14 
error3 = Mult ast13 ast24
error4 = Print error1 
error5 = Sep error4 error1
