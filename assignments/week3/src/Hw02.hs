module Hw02 where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)



-- Part A: Practice defining functions on Integers
--         using if-then-else, == on integers, guards,
--         and where clauses to hide helper functions,
--         plus have all normal functions on Integers.

-- fibonnaci
-- example: fib 0 = 0
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x
  | (mod x 2) == 0 = (fib (x `div` 2))*(2*(fib ((x `div` 2)+1))-(fib (x `div` 2)))
  | (mod (x-1) 2)== 0 = (let a = (fib (((x-1) `div` 2)+1)) in (let b = (fib ((x-1) `div` 2)) in a*a+b*b))
  | True = (fib (x-1))+(fib (x-2))

-- Greatest Common Divisor

gcd :: Integer -> Integer -> Integer
gcd a 0 = 0
gcd 0 a = 0
gcd a b
  | ((mod a b) /= 0) = gcd b (mod a b)
  | True = b



-- Part B: Practice functions on lists


data List a = Nil | Cons a (List a) deriving Show

isEmpty :: List a -> Bool
isEmpty Nil  = True
isEmpty _ = False

length :: List a -> Integer
length Nil = 0
length (Cons h t) = 1+length t

-- Concatenate two lists, make sure it works
-- if either or both are empty
-- Hint: structural induction on first argument
-- you may have done this in the last hw
(++) :: (List a) -> (List a) -> (List a)
(++) a  Nil = a
(++) Nil a = a
(++) (Cons a Nil) (Cons b c) = (Cons a (Cons b Nil)) ++ c
(++) (Cons a (Cons b c)) (Cons d e) = Cons a (Cons b (c ++ (Cons d e)))


-- Add an element to the end of the list
addToEnd :: a -> (List a) -> (List a)
addToEnd a  Nil = Cons a Nil
addToEnd a (Cons h t) = Cons h (addToEnd a t)

-- reverse a list
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons h b) = addToEnd h (reverse b)

-- flatten a list of lists
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a Nil) = a
concat (Cons a b) = a ++ concat b

-- return the first n elements
take :: Integer -> List a -> List a
take n  Nil = Nil
take n (Cons h t)
  | n == 0 = Nil
  | n == 1 = Cons h Nil
  | True = Cons h (take (n-1) t)


-- some "higher order" functions

-- apply the function to each member of the list
map :: (a -> b) ->  List a ->  List b
map f Nil = Nil
map f (Cons h t) = Cons (f h) (map f t)

mhelper :: Integer -> Integer
mhelper a = 7*a

multiplyEachBy7 :: List Integer -> List Integer
multiplyEachBy7 a = map mhelper a


-- keep only the elements from the list that have the property of interest
filter :: (a -> Bool) ->  List a ->  List a
filter  f Nil = Nil
filter  f  (Cons a b)
  | f a = Cons a (filter f b)
  | True = filter f b

isEven :: Integer -> Bool
isEven n
  | (mod n 2) == 0 = True
  | True = False

keepEvens :: List Integer ->  List Integer
keepEvens a = (filter (isEven) a)

-- Pairs

data Pair a b = Pair a b deriving Show


fst :: Pair a b -> a
fst (Pair a b) = a

snd :: Pair a b -> b
snd (Pair a b) = b

-- zip 2 lists together
zip :: (List a) -> (List b) -> List (Pair a b)
zip Nil a = Nil
zip a Nil = Nil
zip  (Cons h1 t1) (Cons h2 t2) = Cons (Pair h1 h2) (zip t1 t2)



-- Maybe

-- sometimes we don't know if we will have a result.  So we can use the "Maybe" datatype.

data Maybe a = Nothing | Just a deriving Show

head :: List a -> Maybe a
head Nil = Nothing
head (Cons h t) = Just h

last :: List a -> Maybe a
last Nil = Nothing
last (Cons h Nil) = Just h
last (Cons h t) = last t


-- Next we define an infix function

-- return nth element in list, starting at 0; similar to array indexing
(!!) :: List a -> Integer -> Maybe a
(Nil)  !! (_) = Nothing
(a) !! (n) = last (take n a)


-- sorting

data Comparison = LessThan | EqualTo | GreaterThan deriving Show

compareInteger :: Integer -> Integer -> Comparison
compareInteger n1 n2
  | n1 < n2 = LessThan
  | n1 == n2 = EqualTo
  | True = GreaterThan

-- when false < true
compareBool :: Bool -> Bool -> Comparison
compareBool b1 b2
  | b1 == b2 = EqualTo
  | b1 < b2 = LessThan
  | True = GreaterThan

-- when we have frequently used functions, we can wrap them in data
data Ordering a = Ordering (a -> a -> Comparison)

intOrd :: Ordering Integer
intOrd = Ordering compareInteger

boolOrd :: Ordering Bool
boolOrd = Ordering compareBool

-- let's write insertion sort

comComp :: Comparison -> Comparison -> Bool
comComp LessThan LessThan = True
comComp EqualTo EqualTo = True
comComp GreaterThan GreaterThan = True
comComp _ _ = False

-- inserts an a into a sorted list of a (the list is sorted least to greatest)
-- for example: insert intOrd 3 (Cons 1 (Cons 4 Nil)) = (Cons 1 (Cons 3 (Cons 4 Nil)))
insert :: Ordering a -> a -> List a -> List a
insert (Ordering f) a Nil = Cons a Nil
insert (Ordering f) a (Cons h Nil)
  | (comComp (f a h) GreaterThan) = Cons h (Cons a Nil)
  | True = Cons a (Cons h Nil)
insert (Ordering f) a (Cons h t)
  | (comComp (f a h) GreaterThan)= Cons h (insert (Ordering f) a t)
  | True = Cons a (Cons h t)


-- sort the list
sort :: Ordering a -> List a -> List a
sort (Ordering f) Nil= Nil
sort (Ordering f) (Cons a Nil) = (Cons a Nil)
sort (Ordering f) (Cons a (Cons b c)) = insert (Ordering f) a (sort (Ordering f) (Cons b c))

-- write a datatype representing a student, the student should have a bu-ID, current year
-- CS students should have a Bool (are they taking 320)
-- Math students have an Integer  (how many friends do they have)

data Student = Cs Integer Integer Bool | S  Integer Integer | M Integer Integer Integer  deriving Show

-- from buid, year, and if they are taking 320
-- for instance a freshman in this class would be created by, "mkCsStudent 12345678 0 True"
mkCsStudent :: Integer ->  Integer ->  Bool -> Student
mkCsStudent buid year taking = Cs buid year taking


-- from buid, year, and how many friends they have
mkMathStudent :: Integer ->  Integer ->  Integer -> Student
mkMathStudent buid year friends = M buid year friends

-- is this a CS student? (this is to make testing easier)
isCs :: Student ->  Bool
isCs (Cs _ _ _) = True
isCs (M _ _ _) = False
isCs (S _ _) = False

isMath :: Student ->  Bool
isMath (M _ _ _) = True
isMath (Cs _ _ _) = False
isMath (S _ _) = False

getBuId :: Student ->  Integer
getBuId (Cs a b c) = a
getBuId (S a b) = a
getBuId (M a b c) = a

getYear :: Student ->  Integer
getYear (Cs a b c) = b
getYear (S a b) = b
getYear (M a b c) = b

compareStu :: Student -> Student -> Comparison
compareStu a b
  | (getBuId a) > (getBuId b) = GreaterThan
  | (getBuId a) == (getBuId b) = EqualTo
  | True = LessThan

stuOrd :: Ordering Student
stuOrd = Ordering compareStu

take320 :: Student -> Bool
take320 (Cs _ _ True )= True
take320 _ = False

-- coolestStudent is defined to be the first CS student who is taking 320 in this list
coolestStudent :: List Student -> Maybe Student
coolestStudent Nil = Nothing
coolestStudent (Cons a Nil) = case a of
                                (Cs b c True ) -> Just a
                                (Cs b c False ) -> Nothing
                                _ -> Nothing
coolestStudent (Cons a b) = head (filter take320 (Cons a b))

-- the students need to be put into pairs for group projects.
-- Take the list of students and create pairs of math and CS students from the list.
-- If possible every Math student should be paired with a CS student.
-- If some students are not paired up that is ok.
groupProject :: List Student -> List (Pair Student Student)
groupProject ls = zip (filter isCs ls) (filter isMath ls)


-- define an ordering on students based entirely on bu-ID, students with the same BU id are "EqualTo" regardless of other information
studentOrd :: Ordering Student
studentOrd = Ordering compareStu
