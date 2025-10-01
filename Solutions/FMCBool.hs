module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum x
      | x == 0 = False
      | x == 1 = True
      | otherwise = error "toEnum: input must be 0 or 1"

    fromEnum True = 1
    fromEnum False = 0

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True  && True  = True
_     && _     = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
x /|\ y = not (x && y)

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
x \|/ y = not (x || y)

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True  <=/=> False = True
False <=/=> True  = True
_     <=/=> _     = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True  = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse = undefined

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>


