module Hw where
import Prelude(Show, undefined)
-- HOMEWORK ONE     Due 2/5 by 11:59pm by upload to your repo
--    (with a couple of hours "grace period" before it is considered late)
--    Note: There is also an analytical part to the homework
--    which will be posted on the class web page, and due to be
--    uploaded to Gradescope with the same due date and time, and
--    with the same grace period.

-- Fill in the bodies of the undefined functions and data.
-- DO NOT CHANGE THE TYPE SIGNATURES!

-- Think about whether you need to write each function
-- recursively on the structure of the data, or can
-- define more simply in terms of previously-defined functions,
-- or use a helper function.
-- You may always add your own helper functions and helper data!

-- Remember: Constructors must be capitalized; variable
-- and function names must be in lower case.
-- Constructor constants are like 0-ary functions (no arguments).


-- Part A: Basic Boolean data and functions

-- Note on data declarations: "deriving Show" will allow
-- data values to be printed by interpreter.


data Bool = True | False     deriving Show

-- Define the following familiar functions on Bools.
-- You may need multiple cases for each one.

not :: Bool -> Bool
not True = False
not False = True


and :: Bool -> Bool -> Bool
and True True = True
and True False = False
and False True = False
and False False = False


or :: Bool -> Bool -> Bool
or True True = True
or True False = True
or False True = True
or False False = False


xor :: Bool -> Bool -> Bool
xor True True  =  False
xor False False  =  False
xor True False = True
xor False True = True
-- Part B: Encoding of natural numbers using data expressions
-- and defining basic functions on these expressions.

data Nat =  Zero | Succ Nat deriving Show

-- the first 6 numbers
zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

three  :: Nat
three = Succ two

four  :: Nat
four = Succ three

five  :: Nat
five = Succ four


-- Write the following functions
-- (Hint: try recursing on structure of first argument)

add ::  Nat -> Nat -> Nat
add Zero Zero  = zero
add Zero n0 = n0
add n00 Zero = n00
add n1 (Succ n2) = add (Succ n1) n2

mult ::  Nat -> Nat -> Nat
mult Zero _  = Zero
mult _ Zero = Zero
mult (Succ Zero) n = n
mult n (Succ Zero) = n
mult n1 (Succ n2) = add n1 (mult n1  n2)

exp ::  Nat -> Nat -> Nat
exp Zero Zero = Succ Zero
exp _ Zero = Zero
exp Zero _  = Succ Zero
exp (Succ n2) n1 = mult (exp n2 n1) n1

-- When are 2 Nats equal?
eq :: Nat -> Nat -> Bool
eq Zero Zero  = True
eq Zero (Succ n) = False
eq (Succ n) Zero = False
eq (Succ n1) (Succ n2) = eq n1 n2

-- When are 2 Nats not equal?
ne :: Nat -> Nat -> Bool
ne n1 n2  = not (eq n1 n2)

-- Less than on Nats
lt :: Nat -> Nat -> Bool
lt Zero Zero = False
lt Zero (Succ x)  = True
lt (Succ x) Zero = False
lt (Succ n1) (Succ n2) = lt n1 n2

-- Remaining Boolean tests

le :: Nat -> Nat -> Bool
le x y = or (lt x y) (eq x y)

gt :: Nat -> Nat -> Bool
gt x y = (not (le x y))

ge :: Nat -> Nat -> Bool
ge x y = not (lt x y)

-- Example of useful test on Nats
-- return True on even Nats, False on odd.
isEven :: Nat -> Bool
isEven Zero = True
isEven (Succ Zero) = False
isEven (Succ (Succ x)) = isEven x

--Return the maximum of two Nats
max :: Nat -> Nat -> Nat
max Zero (Succ x)  = Succ x
max Zero Zero = Zero
max (Succ x) Zero = Succ x
max (Succ x) (Succ y) = (Succ (max x y))


-- Part C:  Data Expressions: Now let's write our own data.

-- C.1: Dates

-- Write a data type for the 7 days of the week
data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday-- = ...  deriving Show

-- what is your favorite Day? (Your choice!)
favoriteDay :: DayOfWeek
favoriteDay = Monday

-- write a function that returns true if it is a weekend
isWeekend :: DayOfWeek -> Bool
isWeekend  Saturday     = True
isWeekend Sunday = True
isWeekend _ = False

-- Write a function that gives the next day
nextDay :: DayOfWeek -> DayOfWeek
nextDay  Monday     = Tuesday
nextDay  Tuesday = Wednesday
nextDay  Wednesday     = Thursday
nextDay  Thursday     = Friday
nextDay  Friday     = Saturday
nextDay  Saturday     = Sunday
nextDay  Sunday     = Monday

-- write a data type for the Months of the year
data Month = Jan | Feb | Mar | Apr | May | Jun | July | Aug | Sep | Oct | Nov | Dec deriving Show

-- In which month is your birthday?
partyMonth :: Month
partyMonth = Nov


-- Write a function that gives the next Month
nextMonth :: Month -> Month
nextMonth  Jan     = Feb
nextMonth  Feb     = Mar
nextMonth  Mar     = Apr
nextMonth  Apr     = May
nextMonth  May     = Jun
nextMonth  Jun     = July
nextMonth  July     = Aug
nextMonth  Aug     = Sep
nextMonth  Sep     = Oct
nextMonth  Oct     = Nov
nextMonth  Nov     = Dec
nextMonth  Dec     = Jan



-- C.2: Cartesian Coordinates

-- Write a data type for a 2D point where x and y are Nats
data Point = XY Nat Nat deriving Show

-- Take 2 Nats and construct a Point
makePoint :: Nat -> Nat -> Point
makePoint a b = XY a b

-- Select components from a Point
getX :: Point -> Nat
getX (XY a b) = a

getY :: Point -> Nat
getY (XY a b) = b


-- The Manhattan distance is the distance in the x direction plus the distance in the y direction
-- for instance the Manhattan distance of points (2,5) and (3,1) is 5
cond True a b = a
cond False a b = b

distance :: Nat -> Nat -> Nat
distance n Zero = n
distance Zero n = n
distance (Succ a) (Succ b) = cond (gt (Succ b) (Succ a)) (distance (Succ b) (Succ a)) (distance a b)


manhattanDistance :: Point -> Point -> Nat
manhattanDistance (XY a b) (XY c d) = add (distance a c) (distance b d)


-- C.3: More Alterative data

-- Assume there is an boring math class where students only answer with a Bool OR with a Nat,
-- write a data type for that answer (hint: you may use two alternatives with | )
data ShortAnswer = S1 Bool | S2 Nat  deriving Show

-- Make a Nat answer
answerNat :: Nat -> ShortAnswer
answerNat a = S2 a

-- Make a Bool answer
answerBool :: Bool -> ShortAnswer
answerBool a = S1 a

-- What is 100 - 99?
ans1 :: ShortAnswer
ans1 = S2 one

-- Is 100 - 99 an odd number?
ansTrue :: ShortAnswer
ansTrue = S1 True

-- If the answers are equal return true otherwise return false
gradeAnswer :: ShortAnswer -> ShortAnswer -> Bool
gradeAnswer (S1 a) (S1 b) = not (xor a b)
gradeAnswer (S2 a) (S2 b) = eq a b
gradeAnswer _ _ = False

-- Part D: Important data structures: Lists

-- D.1: Lists of Nats

-- We can write lists for specific data, let's do Nats first
data ListNat = NilNat | ConsNat Nat ListNat    deriving Show

-- Create a list of the first 4 nats
exampleListNat :: ListNat
exampleListNat = ConsNat one (ConsNat two (ConsNat three (ConsNat four NilNat)))

-- Find the length of a list (remember length is defined as the number of elements in the list)
lengthOfListNat :: ListNat -> Nat
lengthOfListNat NilNat  = Zero
lengthOfListNat (ConsNat a NilNat) = Succ Zero
lengthOfListNat (ConsNat a b) = Succ (lengthOfListNat b)

-- Write a function that finds the sum of all the numbers in the list
sum :: ListNat -> Nat
sum NilNat = Zero
sum (ConsNat a b)     = add a (sum b)

-- Write a function that tells when 2 Nat lists are equal
eqList :: ListNat -> ListNat -> Bool
eqList NilNat NilNat = True
eqList (ConsNat a b) (ConsNat c d)  = and (eq a c) (eqList b d)
eqList _ _ = False

-- Write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member a NilNat                = False
member a (ConsNat b c) = or (eq a b) (member a c)


-- D.2:  Now let's do lists of Bools

data ListBool = NilBool | ConsBool Bool ListBool deriving Show

-- Give a list containing every bool
exampleListBool :: ListBool
exampleListBool = ConsBool True (ConsBool False NilBool)


lengthOfListBool :: ListBool -> Nat
lengthOfListBool NilBool  = Zero
lengthOfListBool (ConsBool a NilBool) = Succ Zero
lengthOfListBool (ConsBool a b) = Succ (lengthOfListBool b)


-- D.3:  General lists: It gets very tiresome to write a list for every single datatype
-- so let's abstract out the type of elements using a polymorphic type

data List a = Nil | Cons a (List a)    deriving Show

-- Write a list of all the Bool values
listOfBool :: (List Bool)
listOfBool = Cons True (Cons False Nil)

-- Write a list of the first three Nats
listOfNat :: (List Nat)
listOfNat = Cons one (Cons two (Cons three Nil))

-- Write a list of all the weekdays
listOfWork :: (List DayOfWeek)
listOfWork = Cons Monday (Cons Tuesday (Cons Wednesday (Cons Thursday (Cons Friday Nil))))

-- Useful function on lists
length :: (List a) -> Nat
length Nil  = Zero
length (Cons a Nil) = Succ Zero
length (Cons a b) = Succ (length b)

-- Part E: Binary trees

-- A binary tree is either empty, or a node with a left subtree
-- a value at the root and a right subtree
data Tree a = Null | Node (Tree a) a (Tree a)     deriving Show

-- Give a balanced tree of three Bools corresponding to
--             True
--            /    \
--        False    False

exampleTree :: Tree Bool
exampleTree = Node (Node Null False Null) True (Node Null False Null)
mytree = Node (Node Null one Null) two (Node (Node Null three Null) (four) (Node Null five Null))

-- return the number of elements in the tree
size :: (Tree a) -> Nat
size Null = Zero
size  (Node Null a Null)  = Succ Zero
size (Node b a c) = add (size b) (add (size c) (Succ Zero))

-- Return the height (= number of nodes in longest path from root to leaf)

height :: (Tree a) -> Nat
height  Null  = Zero
height (Node Null a Null) = Succ Zero
height (Node b a c) = Succ (max (height b) (height c))

-- Do an inorder traversal and store elements in a list
concatenate :: (List a) -> (List a) -> (List a)
concatenate Nil Nil = Nil
concatenate (Cons a b) Nil = (Cons a b)
concatenate Nil (Cons a b) = (Cons a b)
concatenate (Cons a Nil) (Cons b c) = concatenate (Cons a (Cons b Nil)) c
concatenate (Cons a (Cons b c)) (Cons d e) = Cons a (Cons b (concatenate c (Cons d e)))

inorder :: (Tree a) -> (List a)
inorder  Null  = Nil
inorder (Node Null a Null) = Cons a Nil
inorder (Node b a c) = concatenate (concatenate (inorder b) (Cons a Nil)) (inorder c)

-- Do a preorder traversal


preorder :: (Tree a) -> (List a)
preorder Null = Nil
preorder (Node Null a Null)  = Cons a Nil
preorder (Node b a c) = concatenate (concatenate (Cons a Nil) (preorder b)) (preorder c)

-- extra ungraded questions below

-- What is the smallest datatype you can come up with?
data Smallests a = Bit a deriving Show

exampleSmallest = Bit one

-- what is the craziest datatype you can come up with?
data Craziests a = Me a deriving Show

exampleCraziests = Me one
