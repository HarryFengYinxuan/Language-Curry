module TypeclassProblems where

-- define data type for all 7 days of the week
data DayOfTheWeek = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving Show

-- often built-in haskell classes come with nice syntax, when the following is defined try this
-- in the console [Monday .. Friday].

-- Note: do NOT uncomment the types for these functions,
-- they are given in the class declaration, so all you need to do is give the definition. The type is
-- just for reference.

instance Enum DayOfTheWeek where
  --toEnum :: Integer -> DayOfTheWeek (start counting at 0)
  toEnum 0 = Mon
  toEnum 1 = Tue
  toEnum 2 = Wed
  toEnum 3 = Thu
  toEnum 4 = Fri
  toEnum 5 = Sat
  toEnum 6 = Sun
  toEnum n = undefined

  --fromEnum :: DayOfTheWeek -> Integer
  fromEnum Mon  = 0
  fromEnum Tue  = 1
  fromEnum Wed  = 2
  fromEnum Thu  = 3
  fromEnum Fri  = 4
  fromEnum Sat  = 5
  fromEnum Sun  = 6



-- First we will work with a custom type class that makes an example of a type
class HasExample a where
  example :: a

-- finish up the instances on these types
instance HasExample DayOfTheWeek where
  example = Sun


instance HasExample Bool where
  example = True

instance HasExample Integer where
  example = 1

instance HasExample [a] where
  example = []

instance (HasExample a, HasExample b) => HasExample (a,b) where
  example = (example, example)

instance HasExample b => HasExample (a -> b) where
  example = \x->example

iSureWishIHadThisFunction :: Integer -> Bool -> (a ->b ) -> (Integer, (Bool, DayOfTheWeek))
iSureWishIHadThisFunction = example -- it's a little silly but the code is automatically generated!


-- next we will work with a custom type class that gives all the things

class AllTheThings a where
  listOfAll :: [a]
-- laws: finite, no repeat

-- when we have this defined we can check that ALL inputs of a function are correct
forAll :: AllTheThings a => (a -> Bool) -> Bool
forAll f = all f listOfAll


instance AllTheThings Bool where
  listOfAll = [True, False]


boolEq  :: Bool -> Bool
boolEq = (\b  -> b == b)

--try "forAll boolEq" in the console!

-- finish up the instances on these types
instance AllTheThings DayOfTheWeek where
  listOfAll = [Mon .. Sun]

eleTuples :: a -> [b] -> [(a, b)]
eleTuples a [] = []
eleTuples a (b:bs) = [(a, b)] ++ (eleTuples a bs)

allTuples :: [a] -> [b] -> [(a,b)]
allTuples [] [] = []
allTuples [] a = []
allTuples a [] = []
allTuples (a:as) (b:bs) = (eleTuples a (b:bs)) ++ (allTuples as (b:bs))

instance (AllTheThings a, AllTheThings b) => AllTheThings (a,b) where
  listOfAll = allTuples (listOfAll) (listOfAll)



-- Ungraded bonus challenge problems!

instance (AllTheThings a, Eq a, AllTheThings b) => AllTheThings (a -> b) where
  listOfAll = undefined

instance (AllTheThings a, Show a, Show b) => Show (a -> b) where
  show f = undefined
