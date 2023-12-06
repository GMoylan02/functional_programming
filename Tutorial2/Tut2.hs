module Tut2 where
import Data.Char (toUpper)

name, idno, username :: String
name      =  "Gerard Moylan"  -- replace with your name
idno      =  "21364007"    -- replace with your student id
username  =  "gmoylan"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

mknested 0 = []
mknested n = [1] : mknested (n-1)

test = [[1],[2],[3],[4]]

{- Part 1

Write `lcat` to concatenate lists left-first

lcat [xs1,xs2,xs3,xs4] = ((xs1++xs2)++xs3)++xs4

-}
lcat :: [[a]] -> [a]
lcat [] = []
lcat [xs] = xs
-- lcat (xs:ys:xss) = (xs++ys)++lcat xss -- (xs1++xs2)++(xs3++xs3)++...
lcat (xs:ys:xss) = lcat ((xs++ys):xss)

{- Part 2

Write `rcat` to concatenate lists right-first

rcat [xs1,xs2,xs3,xs4] = xs1++(xs2++(xs3++xs4))

-}
rcat :: [[a]] -> [a]
rcat [] = []
rcat (xs:xss) = xs ++ (rcat xss)

