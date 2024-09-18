import Control.Parallel

quicksort []     = []
quicksort [x]    = [x]
quicksort (x:xs) =
  leftpartition `par` rightpartition `par` (leftpartition ++ (x:rightpartition))
  where 
      leftpartition = quicksort [y|y <- xs, y < x]
      rightpartition = quicksort [y|y <- xs, y >= x]

--quicksort that uses forcelist to force evaluation
newSort []     = []
newSort [x]    = [x]
newSort (x:xs) =
  (forceList leftpartition) `par` (forceList rightpartition) `par` (leftpartition ++ (x:rightpartition))
  where 
      leftpartition  = newSort [y | y <- xs, y < x]
      rightpartition = newSort [y | y <- xs, y >= x]

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `seq` forceList xs

main = print $ newSort [6,54,1,2,5,5,43,31,12,67,5,234,32,23,23,23,435,3,12,14,675,4,67,5,34,23,2,23,32,3,4,6,56,54,645,6,456,45,6,546,23]