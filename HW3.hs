{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
serialize :: Tree Int -> [Int]
serialize Empty = [-1]
serialize (Tree left x right) = [x] ++ serialize left ++ serialize right

deserialize :: [Int] -> Tree Int
deserialize [] = Empty
deserialize list = fst (deserialize' list)
              where
                deserialize' :: [Int] -> (Tree Int, [Int])
                deserialize' (-1 : xs) = (Empty, xs)
                deserialize' (x : xs) =
                  let (left, rest) = deserialize' xs
                      (right, rest') = deserialize' rest
                  in (Tree left x right, rest')
                deserialize' [] = error "deserialize' called with an empty list"

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend [] il = il
iprepend (x:xs) il = x :> iprepend xs il

itake :: Integer -> InfiniteList a -> [a]
itake n _ | n <= 0 = []
itake n (x :> xs) = x : itake (n - 1) xs

idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop n il@(_ :> _) | n <= 0 = il
idrop n (_ :> xs) = idrop (n - 1) xs

naturals :: InfiniteList Integer
naturals = iiterate (+1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter predicate (x :> xs) = if predicate x then x :> ifilter predicate xs else ifilter predicate xs

ifind :: (a -> Bool) -> InfiniteList a -> a
ifind predicate (x :> xs) = if predicate x then x else ifind predicate xs

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (xs :> xss) = iprepend xs (iconcat xss)

integers :: InfiniteList Integer
integers = 0 :> iconcat (imap (\x -> [x, negate x]) (iiterate (+1) 1))

--rationals :: InfiniteList Rational
--rationals = foldrRationals (iiterate (\ x -> x + 1 ) 1) where
--            foldrRationals (n :> ns) = n % 1 :> (1 % n) :> foldrRationals ns



fromListToInfinity :: [a] -> InfiniteList a
fromListToInfinity [] = error "Cannot convert an empty list to an InfiniteList"
fromListToInfinity (x:xs) = x :> fromListToInfinity xs

igcd :: Integer -> Integer -> Integer
igcd a 0 = abs a
igcd a b = igcd b (a `mod` b)

-- Generate all pairs of integers (m, n) where n != 0
allPairs :: InfiniteList (Integer, Integer)
allPairs = fromListToInfinity [(m, n) | n <- [1..], m <- [-(n+20)..n+20], n /= 0]

-- Create an infinite list of all rational numbers
rationals :: InfiniteList Rational
rationals = normalizeRationals allPairs

-- Normalize and ensure each rational is unique and reduced
normalizeRationals :: InfiniteList (Integer, Integer) -> InfiniteList Rational
normalizeRationals ((m, n) :> rest)
  | igcd m n == 1 = (m % n) :> normalizeRationals rest
  | otherwise    = normalizeRationals rest


-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = rationals

-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)
data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)
parseAndRun :: String -> Either RunError [Int]
parseAndRun = undefined
