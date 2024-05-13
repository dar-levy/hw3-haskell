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
serialize Empty = [-1]  -- Use -1 to indicate an Empty node
serialize (Tree left x right) = [x] ++ serialize left ++ serialize right

deserialize :: [Int] -> Tree Int
deserialize [] = Empty
            deserialize list = fst (deserialize' list)
                          where
                            deserialize' :: [Int] -> (Tree Int, [Int])
                            deserialize' (-1 : xs) = (Empty, xs)  -- Encounter -1, return Empty and the rest of the list
                            deserialize' (x : xs) =
                              let (left, rest) = deserialize' xs
                                  (right, rest') = deserialize' rest
                              in (Tree left x right, rest')  -- Construct tree node with left and right subtrees
                            deserialize' [] = error "deserialize' called with an empty list; input list was too short."

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList
itoList :: InfiniteList a -> [a]
itoList = undefined
iiterate :: (a -> a) -> a -> InfiniteList a
iiterate = undefined
irepeat :: a -> InfiniteList a
irepeat = undefined
iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend = undefined
itake :: Integer -> InfiniteList a -> [a]
itake = undefined
idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop = undefined
naturals :: InfiniteList Integer
naturals = undefined
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap = undefined
ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter = undefined
ifind :: (a -> Bool) -> InfiniteList a -> a
ifind = undefined
iconcat :: InfiniteList [a] -> InfiniteList a
iconcat = undefined
integers :: InfiniteList Integer
integers = undefined
rationals :: InfiniteList Rational
rationals = undefined
-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = undefined

-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)
data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)
parseAndRun :: String -> Either RunError [Int]
parseAndRun = undefined
