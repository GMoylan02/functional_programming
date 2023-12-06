module Tut3 where
import Data.Char (toUpper)

data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)
  deriving Show

{- Part 1
Write `binsert` to insert items into a BinTree
-}
binsert = error "deez"

{- Part 2
Write `blookup` to search a BinTree
-}
blookup = error "blookup NYI!"

{- Part 3
Write `tpose` to transpose a list of lists
-}
-- 

tpose :: [[a]] -> [[a]]
tpose [] = []
tpose (x:xs) = head x ++ tpose xs : tpose (tail x : tail xs)
--tpose = error "tpose NYI!"
