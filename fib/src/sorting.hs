
import Control.Parallel

quicksort []     = []
quicksort [x]    = [x]
quicksort (x:xs) =
 leftpartition `par` rightpartition `par` (leftpartition ++ (x:rightpartition))
  where 
    leftpartition = quicksort [y|y <- xs, y < x]    
    rightpartition = quicksort [y|y <- xs, y >= x] 

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `seq` forceList xs

main = print $ quicksort [6,54,1,2,5,5,43,31,12]