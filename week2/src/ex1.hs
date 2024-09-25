import System.IO

--Q1
f1::[IO a] -> IO a
f1 (x:xs) = foldl (>>) x xs

----------------------------------------------------------------------------------------

--Q2
--main =  f1 actions   
--        where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']

-- This prints "hello", since f1 does each of the putChars in the above list in sequence

----------------------------------------------------------------------------------------

--Q3
--main = do
--         let a = f1 actions
--             b = if True then putChar 'a' else putChar 'b'
--         putStr "Hi there"          
--       where actions = [do putChar 'h', putChar 't']

-- this only prints "Hi there".
-- a is bound to f1 over the list actions, but this is never evaluated because of laziness so nothing in actions is ever printed
-- b is a similar case, it is bound to putChar 'a' which is never evaluated because b is never used.

----------------------------------------------------------------------------------------

--Q4
while :: IO Bool -> IO ()
while a = do
    response <- a
    if response 
        then while a
    else return ()

--main :: IO ()
--main = do
--  putStrLn "type no to end loop."
--  while keepGoing
--  putStrLn "finished loop"
  
keepGoing :: IO Bool
keepGoing = do
  putStrLn "(yes/no)"
  response <- getLine
  return (response /= "no")
-- the above main and keepGoing functions are for testing the 'while' function

----------------------------------------------------------------------------------------

--Q5
f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 (x:xs) = do
    z <- x
    zs <- f2 xs
    return (z:zs)

read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions

main = do a <- read10
          putStr a
          
-- this is for testing f2 and read10
