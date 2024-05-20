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
parseAndRun s = parseAndRunAux (lines s) (Right [])

parseAndRunAux :: [String] -> Either StackError [Int] -> Either RunError [Int]
parseAndRunAux [] (Right stack) = Right stack
parseAndRunAux _ (Left stack) = Left (InstructionError stack)
parseAndRunAux (s : strs) (Right stack)

 -- Empty line
 | s == "\n" || s == "" = parseAndRunAux strs (Right stack)

 -- Instrruction is not defined
 -- | isNothing instruction = Left (ParseError s)
 
 -- Check if the instruction is valid, if not, raise a ParseError
 | otherwise = case instruction of
    Just inst -> parseAndRunAux strs (runInstruction stack inst)
    Nothing -> Left (ParseError s)
 where instruction = parseInstructions s

readInteger :: String -> Maybe Int
readInteger = readMaybe

-- for debugging
data Instruction = Instruction {inst :: String, val :: Maybe Int} deriving (Show, Eq)
--data Instruction = Instruction {inst :: String, val :: Maybe Int}

parseInstructions :: String -> Maybe Instruction
parseInstructions str = case words str of
  ["POP"] -> Just (Instruction "POP" Nothing)
  ["SWAP"] -> Just (Instruction "SWAP" Nothing)
  ["DUP"] -> Just (Instruction "DUP" Nothing)
  ["ADD"] -> Just (Instruction "ADD" Nothing)
  ["SUB"] -> Just (Instruction "SUB" Nothing)
  ["MUL"] -> Just (Instruction "MUL" Nothing)
  ["DIV"] -> Just (Instruction "DIV" Nothing)
  ("PUSH" : rest : _) -> case val of
    Just _ -> Just (Instruction "PUSH" val)
    Nothing -> Nothing
    where val = readInteger rest
  _ -> Nothing

runInstruction :: [Int] -> Instruction -> Either StackError [Int]

-- If we recieved Just in the value member of Instruction, then it must be a PUSH instruction, because this is the only instrction that has a value attached to it,
-- And we already checked if the instruction is a valid one
runInstruction stack (Instruction _ (Just val)) = Right (val : stack)

-- If we have an empty stack, and got intruction other than PUSH (see above comment why is that), we raise an error
runInstruction [] (Instruction inst Nothing) = Left (StackUnderflow inst Nothing)

-- Got SWAP instruction, but there is only one element in the stack
runInstruction [x] (Instruction "SWAP" Nothing) = Left (StackUnderflow "SWAP" (Just x))
runInstruction (x : y: stack) (Instruction "SWAP" Nothing) = Right (y : x : stack)

-- At least one element in the stack, and not SWAP or PUSH
runInstruction (top : stack) (Instruction inst Nothing) = case inst of
  "POP" -> Right stack
  "DUP" -> Right (top : top : stack)
  "ADD" -> runArithmaticInstructions "ADD" (+) (top : stack)
  "SUB" -> runArithmaticInstructions "ADD" (-) (top : stack)
  "MUL" -> runArithmaticInstructions "MUL" (*) (top : stack)
  "DIV" -> runArithmaticInstructions "DIV" div (top : stack)
  --"SWAP" -> runArithmaticInstructions "SWAP" (\x y -> [y, x]) (top : stack)

  -- We should never get to this section, as we already checked if the instructions are valid and we covered all the instructions
  _ -> Left (StackUnderflow inst (Just top))


runArithmaticInstructions :: String -> (Int -> Int -> Int) -> [Int] -> Either StackError [Int]
runArithmaticInstructions op _ [] = Left (StackUnderflow op Nothing)
runArithmaticInstructions op _ [x] = Left (StackUnderflow op (Just x))
runArithmaticInstructions "DIV" _ (_ : 0 : _) = Left DivisionByZero
runArithmaticInstructions _ f (x : y : seq) = Right ((f x y) : seq)