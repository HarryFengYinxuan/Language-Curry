module UsingLambdaCalc  where
import Prelude (undefined,Show) -- DO NOT CHANGE THIS LINE


-- λ-calculus
--
-- For these problems you will only be allowed to use lambda expressions.  Nothing else is allowed.
--

-- Lambda Calculus in Haskell
--
-- The λ-calculus is a fundamental notion in computer science and has become part of the culture of
-- programming languages. So much so that many languages, including Java, Python and Haskell, include
-- lambda expressions in their syntax.
--
--
-- For instance, the lambda expression
--
-- λx. λy. (x y)
--
-- can be written in Haskell as
--
-- \ x -> \ y -> x y
--
-- because Haskell only allows "well-typed" functions some valid lambda expressions are not allowed
--
--  λx. (x x)
--
-- none of the examples in this assignment require that

-- Let's explore the λ-calculus in Haskell by exploring how some primitive ideas, such as boolean values,
-- conditions, and integers can be encoded as λ-expressions.

-- ##  Booleans
--
-- The basic values in Boolean logic are the constants True and False. These can be encoded as λ-expressions as follows:
true = \ x -> \ y -> x
false = \ x -> \ y -> y

-- Notice that true takes two arguments and returns the first of them; false takes two arguments and returns the second of them:

-- true <then> <else>  --> <then>
--
-- false <then> <else> --> <else>
--
-- where --> indicates beta-conversion as discussed in lecture.


-- Now we consider how to define the standard Boolean operators. The not operator must be a λ-expression such that
--
--             not true --> false
--
-- and
--
--             not false --> true
--
--
-- Here is the way to do it:

not' =  \bool -> bool false true

-- However we will use this equivalent formulation to make Haskell's type inference happy
not =  \bool -> \t -> \f -> bool f t


-- Your turn!  Try to implement the Boolean operator for and.
and = \a -> \b -> a b false



-- Now implement the or operator:
or = \p -> \q -> p true q



-- Implement an "xor"


-- xor = \c1 -> \c2 -> (or (and c1 (not c2))) ((and c2) (not c1))
xor = \p -> \q -> p (not q) q


--
-- ##  Natural Numbers
--
-- Another common construct is a number.  The number n can be represented by the n-fold application of a
-- function to an argument. For instance we can represent 2 as

two = \ f -> \ start -> f (f start)


-- Try to implement the following numbers

zero = \f -> \start -> start
one = \f -> \start -> f start

three = \f -> \start -> f (f (f start))



-- Implement the number 7:

-- seven = \f -> \start -> (f (f (f (f (f (f (f start)))))))
seven = add three (add three one)


-- Now, how would we represent addition? To represent n+m, we would have to compose an n-fold application
-- with an m-fold application:

add = \m -> \n -> \f -> \x -> m f (n f x)


-- Now implement the multiplication function

mult = \m -> \n -> \f -> m (n f)


-- Finally, implement a function that tests if a number is even

isEven = \a -> a not true



-- ##  Bonus:  Recursion (ungraded)

-- One important aspect of the lambda calculus is the ability to perform arbitrary recursion. It is not obvious from our definition of lambda calculus that this is possible. Try to figure out a scheme to do this.  You will need to do this work by hand since Haskell’s type system will not allow it.
--
-- Try to use recursion to write a factorial function.
