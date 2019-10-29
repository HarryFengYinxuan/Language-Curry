module Lang2 where

import PrinterMonad

data Ast =
    LiteralInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast
  | Sub Ast Ast
  | Mult Ast Ast

eval :: Ast -> PrinterMonad Integer Integer
eval (LiteralInt x) =  PrinterMonad [] x
eval (Plus a b) =   let (printList1, ret1) = runPrinterMonad (eval a) in
                    let (printList2, ret2) = runPrinterMonad (eval b) in
                    PrinterMonad (printList1++printList2) (ret1+ret2)
eval (Separator a b) =  let (printList1, ret1) = runPrinterMonad (eval a) in
                        let (printList2, ret2) = runPrinterMonad (eval b) in
                        PrinterMonad (printList1++printList2) ret2
eval (Print a) = let (printList, ret) = (runPrinterMonad (eval a)) in  PrinterMonad (printList++[ret]) ret
eval (Sub a b) =  let (printList1, ret1) = runPrinterMonad (eval a) in
                  let (printList2, ret2) = runPrinterMonad (eval b) in
                  PrinterMonad (printList1++printList2) (ret1-ret2)
eval (Mult a b) =   let (printList1, ret1) = runPrinterMonad (eval a) in
                    let (printList2, ret2) = runPrinterMonad (eval b) in
                    PrinterMonad (printList1++printList2) (ret1*ret2)



-- Ungraded bonus: There is a built in monad on tuples with the same functionality
eval' :: Ast -> ([Integer], Integer)
eval' =  undefined

-- show the fully parenthesized syntax
instance Show Ast where
  show (LiteralInt i) = show i
  show (l `Plus` r) = "(" ++ (show l) ++ " + " ++  (show r) ++ ")"
  show (l `Separator` r) = "(" ++ show l ++ "; " ++ show r ++ ")"
  show (Print b) =  "print(" ++ show b ++ ")"
  show (l `Sub` r) = "(" ++ (show l) ++ " - " ++  (show r) ++ ")"
  show (l `Mult` r) = "(" ++ (show l) ++ " * " ++  (show r) ++ ")"
