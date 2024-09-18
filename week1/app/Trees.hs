module Trees where

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
    deriving Show


count :: Tree a -> Integer
count (Leaf _) = 0
count (Node left right) = 1 + count left + count right

--reports the maximum depth of the tree
depth :: Tree a -> Integer
depth (Leaf _) = 0
depth (Node left right) = 1 + max (depth left) (depth right)

--converts the tree to a list
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left right) = flatten left ++ flatten right