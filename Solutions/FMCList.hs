{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x:_) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "tail: empty list"

null :: [a] -> Bool
null (_:_) = False
null [] = True

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs <: x

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "minimum: empty list"
minimum [x] = x
minimum (x:xs)
  |x < minimum xs = x
  |otherwise = minimum xs

maximum :: Ord a => [a] -> a
maximum [] = error "maximum: empty list"
maximum [x] = x
maximum (x:xs)
 |x > maximum xs = x
 |otherwise = maximum xs

take :: Int -> [a] -> [a] 
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n xs | n <= 0 = xs
drop n (_:xs) = drop (n-1) xs

-- takeWhile
-- dropWhile

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)

init :: [a] -> [a]
init [] = error "unit: empty list"
init [_] = []
init (x:xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = xss ++ map (x:) xss
  where xss = subsequences xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs)
  | p x       = True    
  | otherwise = any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs)
  | p x       = all p xs
  | otherwise = False

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y xs = any (\x -> x == y) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x    = True
  | otherwise = elem' y xs


(!!) :: [a] -> Int -> a
[]      !! _ = error "!!: indice fora dos limites"
(x:_)   !! 0 = x
(_:xs)  !! n
  | n < 0     = error "!!: indice negativo"
  | otherwise = xs !! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "cycle: lista vazia"
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

-- replicate

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True    
isPrefixOf _ [] = False 
isPrefixOf (x:xs) (y:ys)
  | x == y    = isPrefixOf xs ys
  | otherwise = False 

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

-- isSuffixOf

zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)
  | x `elem` nub xs = nub xs     
  | otherwise       = x : nub xs

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x:xs)
  | p x       = ([], x:xs) 
  | otherwise = (x:ys, zs) 
  where (ys, zs) = break p xs

-- lines
-- words

unlines :: [String] -> String
unlines [] = []
unlines (x:xs) = x ++ "\n" ++ unlines xs

unwords :: [String] -> String
unwords [] = []
unwords [x] = x
unwords (x:xs) = x ++ " " ++ unwords xs

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s = s == reverse s

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

