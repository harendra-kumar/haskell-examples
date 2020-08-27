{-# LANGUAGE RecursiveDo #-}

import Control.Monad (mzero)
import Control.Monad.Fix (mfix)
import Data.IORef (IORef, newIORef)
import Data.List (unfoldr)

-------------------------------------------------------------------------------
-- Recursion
-------------------------------------------------------------------------------

-- undefined
x = x

-------------------------------------------------------------------------------
-- Recursive functions
-------------------------------------------------------------------------------

-- recursion
recFunc x =
    if x > 1
    then recFunc (x - 1)
    else 1

sum1 [] = 0
sum1 (a:as) = a + sum1 as

fact 0 = 1
fact n = n * fact (n-1)

{-
-- using n+k patterns
fact' 0 = 1
fact' (n+1) = (n+1) * fact' n
-}

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iter = takeWhile (/= 1) $ iterate recFunc 10

-- Iterative Wrapper for Recursion
g = f g

    where

    f k x =
        if x > 1
        then k (x - 1)
        else 1

-------------------------------------------------------------------------------
-- Recursive monadic
-------------------------------------------------------------------------------

-- would print "yes" in a loop
infiniteLoop = do
    putStrLn "yes"
    infiniteLoop
    putStrLn "no"

ioLoop n = do
    putStrLn "yes"
    if n >= 0
    then ioLoop (n - 1)
    else return ()
    putStrLn "no"

-- XXX the behavior when we introduce a recursive loop in it, when the lists
-- have single elements?
-- mzero cuts off the rest of the loop
listLoop = do
    x <- [1,2,3]
    y <- [4,5,6]
    if (x + y < 6)
    then return (x,y)
    else mzero

-------------------------------------------------------------------------------
-- Recursive data/Co-recursion
-------------------------------------------------------------------------------

data F = F F

x1 = F x1

data Nat = Zero | S Nat

data Cofree f a = a :< f (Cofree f a)

-- infinite list
infinite1s =
    let xs = 1 : xs
    in take 10 xs

-- A cyclic infinite list
cyclicList =
    let x = 1 : 2 : 3 : x
    in take 10 x

fibonacci =
    let fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
    in take 10 fibs

-- let, mutually recursive
recursiveListFib =
    let xs = 1 : 1 : rest
        rest = zipWith (+) xs (tail xs)
    in take 10 xs

-- using let
cyclicStructure1 =
  let a = [1, c - 5]
      b = head a + 2
      c = b * 4
  in (a, b, c)

-------------------------------------------------------------------------------
-- Corecursive functions
-------------------------------------------------------------------------------

showStream (x:xs) = show x : showStream xs

-- Sum of a stream
sumSoFar x [] = [x]
-- the second argument is corecursive so we can keep pattern matching on it
sumSoFar x (y:ys) = x : sumSoFar (x+y) ys

cyclicUnfoldr =
  unfoldr (\x -> Just (x, x)) 1

unfoldrBuild =
    let builder (x, y : ys) = Just (x + y, (x + y, ys))
    in unfoldr builder (0, (let x = 1 : x in x))

-------------------------------------------------------------------------------
-- fix
-------------------------------------------------------------------------------

-- Tying open ends of a cyclic definition

fix :: (a -> a) -> a
fix f = let x = f x in x         -- Lambda dropped
fix1 f = f (fix1 f)              -- Lambda lifted

-- The expression is already fixed, fully defined.
fixFixed = let f x = 1 in fix f

-- The value of the expression always remains undefined
-- Cannot be fixed.
fixNonTerminating =
  let f x = if x > 1 then x - 1 else 1
  in fix f

-- Tie back a function, generates a recursive function
-- The g passed is same as f g
fixHigherOrder =
    let f g = \x -> if x > 1 then g (x - 1) else 1
    in fix f 10

-- Tie back part of a structure
infiniteList =
    take 10 $ fix (\xs -> 1 : fmap (+1) xs)

-- Tie back part of a structure, fibonacci numbers
-- using fix
fixFib =
    let f xs = 1 : 1 : zipWith (+) xs (tail xs)
    in take 10 (fix f)

-- using fix
cyclicStructure =
    let f :: ([Int], Int, Int) -> ([Int], Int, Int)
        f ~(a, b, c) = ([1, c - 5], head a + 2, b * 4)
    in fix f

-------------------------------------------------------------------------------
-- MonadFix
-------------------------------------------------------------------------------

-- won't work for strict monads
mfix1 f = fix (>>= f)

-- MonadFix instance of list fixes each element of the list
--
cyclicListElements = do
    let xs = mfix f
    print $ take 10 xs

    where

    -- Each element of the list can be defined in a cyclic fashion. The first
    -- element of each tuple in the list  is defined in terms of the second.
    -- Note: you cannot acheive this by just changing the order the monad
    -- statements because that would change the order in which the list
    -- elements are generated.
    f ~(a,b) = do
          y <- [b + 1, b + 2, b + 3]
          z <- [4,5,6]
          return (y,z)

-- IO MonadFix: An IO action can be defined cyclically
data Node = Node Int (IORef Node)

mknode = mfix $ \p -> do
  p' <- newIORef (Node 0 p)
  putStrLn "node created"
  return p'

-- recursive do
recdo = do
  rec a <- [1, 2, b]
      b <- [3, 4]
  return (a, b)

-- mdo
moodoo = mdo
  a <- [1, 2, b]
  b <- [3, 4]
  return (a, b)

-------------------------------------------------------------------------------
-- Fix
-------------------------------------------------------------------------------

newtype Fix f = In (f (Fix f))

-------------------------------------------------------------------------------
-- Try the examples
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- cyclicList
    -- cyclicListElements
    -- ioLoop 1
    print listLoop
