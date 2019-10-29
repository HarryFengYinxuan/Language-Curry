module PrinterMonad where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function


data PrinterMonad out a = PrinterMonad [out] a deriving Show



-- function that just runs the function contained in PrinterMonad
runPrinterMonad::  (PrinterMonad out a) -> ([out], a)
runPrinterMonad (PrinterMonad ls a) = (ls, a)



instance Functor (PrinterMonad out) where
  -- fmap :: (a -> b) -> PrinterMonad out a -> PrinterMonad out b
  fmap f (PrinterMonad ls a) = PrinterMonad ls $ f a
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (PrinterMonad out) where
  pure = return
  (<*>) = ap

instance Monad (PrinterMonad out) where
  --return :: a -> PrinterMonad out a
  -- for this monad, return should not print anything!
  return a = PrinterMonad [] a

  --(>>=) :: PrinterMonad out a -> (a -> PrinterMonad out b) -> PrinterMonad out b
  -- for this monad, append the 1st print buffer in front of the 2nd
  (PrinterMonad ls a) >>= f = let (PrinterMonad out b) = f a in PrinterMonad (ls++out) b

  -- make sure your implementation follows the Monad laws

-- technical note:
-- this is a special case of what is called the writer monad.
