module BareBonesLast where


import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)

import Control.Monad(ap) -- for the things we will ignore right now

-- Implement the following type class instances over our bare bones data
-- You may add any dependencies you see fit
-- Follow the instructions, your implementations must obey the standerd type class laws.
-- type class laws will explained in lecture on W 2/30 and explored in the analytical part of this homework (if you get stuck here, look at that).
-- Note that sometimes laws and typing rules completely define the implementation,
-- if you don't see instructions look up the laws.


data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

-- Ignore this for now
instance Applicative List where
  pure = return
  (<*>) = ap

concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a Nil) = a
concat (Cons a b) = concatenate a (BareBonesLast.concat b)

concatenate :: (List a) -> (List a) -> (List a)
concatenate Nil Nil = Nil
concatenate (Cons a b) Nil = (Cons a b)
concatenate Nil (Cons a b) = (Cons a b)
concatenate (Cons a Nil) (Cons b c) = concatenate (Cons a (Cons b Nil)) c
concatenate (Cons a (Cons b c)) (Cons d e) = Cons a (Cons b (concatenate c (Cons d e)))

instance Monad List where
  -- returns a singleton list containing x
-- Prelude> (return True) :: [Bool]
-- [True]
-- Prelude> (return 7) :: [Integer]
-- [7]
  return x = Cons x Nil

  -- apply function to every value in the list, and flatten the results (maintaining the order).
  --  [1,2,3] >>= (\x -> [x+10,x+100])     ==    [11,101,12,102,13,103]
  a >>= f = BareBonesLast.concat (fmap f a)



data Maybe a = Nothing | Just a deriving (Show, Eq)


instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

-- Ignore this for now
instance Applicative Maybe where
  pure = return
  (<*>) = ap

instance Monad Maybe where
  -- should be the same as Just
  return x = Just x

  -- on Just, apply the function
-- Prelude> (Just 7) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Just 9
-- Prelude> (Just 3) >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (\ x -> if x == 3 then Nothing else Just (x+2))
-- Nothing
-- Prelude> Nothing >>= (undefined) -- a little wierd
-- Nothing
  (Just x) >>= f = f x
  Nothing >>= f = Nothing


data  Either a b  =  Left a | Right b deriving (Show, Eq)

instance Functor (Either a) where
    fmap f (Right r) = Right (f r)
    fmap f (Left l) = Left l

-- Ignore this for now
instance Applicative (Either a) where
  pure = return
  (<*>) = ap


instance Monad (Either a) where
  return x = Right x

  (Right x) >>= f = f x
  (Left x) >>= f = Left x

-- While Monad (Either a) is completely determined by the types and the laws
-- here are some examples hints, note this is sometimes called the "Error Monad"

-- Prelude> (return 7) :: Either a Integer
-- Right 7
-- Prelude> (Right 3) >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Left "whoops"
-- Prelude> (Right 7) >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Right 9
-- Prelude> (Left "uhoh") >>= (\ x -> if x == 3 then (Left "whoops") else Right (x+2))
-- Left "uhoh"


-- a simple data used mostly for demonstration purposes

data Identity a = Identity a deriving (Show, Eq)

runIdentity (Identity a) = a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- Ignore this for now
instance Applicative Identity where
  pure = return
  (<*>) = ap


instance Monad Identity where
  return x = Identity x

  (Identity x) >>= f = f x


-- a simple data used mostly for demonstration purposes

data Trival a = NoA deriving (Show, Eq)


instance Functor Trival where
    fmap f NoA = NoA

-- Ignore this for now
instance Applicative Trival where
  pure = return
  (<*>) = ap

instance Monad Trival where
  return x = NoA

  NoA >>= f = NoA



-- ungraded bonus

data Pair a b = Pair a b deriving (Show, Eq) -- remember same as Pair a b = (a,b) in standard Haskell

instance Functor (Pair a) where
  fmap f _ = undefined  -- you should have done this in week 5

-- Ignore this for now
instance Monoid a => Applicative (Pair a) where
  pure = return
  (<*>) = ap


-- note that the standard implementation is more interesting and is often called the Writer Monad
--    ([True],7) >>= (\ x -> ([False],x+2))       ==      ([True,False],9)
instance Monoid a => Monad (Pair a) where
  return x = undefined

  _ >>= _ = undefined
