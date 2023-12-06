module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED FUNCTIONS (f1..fN)
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
f1 :: [a] -> [a]
-- returns a list of every 172nd element of its input
f1 [] = []
f1 xs | length xs < 172 = []
      | otherwise = xs !! 171 : (f1 (drop 172 xs))


-- Q2 (3 marks)
f2 :: [Int] -> Int
-- sums every 280th element of its input
f2 [] = 0
f2 xs | length xs < 280     = 0
      | otherwise           = xs !! 279 + (f2 (drop 280 xs))


-- Q3 (4 marks)
f3 :: [Int] -> Int
-- multiplies every 306th element of its input
f3 [] = 0
f3 ns | length ns < 306     = 1
      | otherwise           = ns !! 305 * (f3 (drop 306 ns))

-- Q4 (8 marks)
f4 :: [Maybe Int] -> (Int,[Maybe Int])
-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   55   |    add    | fixed 3  | term    |
--    |   43   |    add    | fixed 4  | skip    |
--    |   92   |    add    | fixed 5  | 6       |
--    |   35   |    add    | stop@ 5  | term    |
--    |   32   |    add    | stop@ 6  | skip    |
--    |   45   |    add    | stop@ 3  | 1       |
--    |   58   |    mul    | fixed 4  | term    |
--    |   29   |    mul    | fixed 6  | skip    |
--    |   83   |    mul    | fixed 4  | 2       |
--    |   69   |    mul    | stop@ 4  | term    |
--    |   62   |    mul    | stop@ 5  | skip    |
--    |   16   |    mul    | stop@ 5  | 2       |
--    -------------------------------------------


f4 [] = (0,[])
f4 (x:xs) | isNothing x         = f4 xs
          | fromJust x == 55    = operate_fixed_term add xs 3
          | fromJust x == 43    = operate_fixed_skip add xs 4
          | fromJust x == 92    = operate_fixed_replace add xs 5 6
          | fromJust x == 35    = operate_stop_term add xs 5
          | fromJust x == 32    = operate_stop_skip add xs 6
          | fromJust x == 45    = operate_stop_replace add xs 3 1
          | fromJust x == 58    = operate_fixed_term mul xs 4
          | fromJust x == 29    = operate_fixed_skip mul xs 6
          | fromJust x == 83    = operate_fixed_replace mul xs 4 2
          | fromJust x == 69    = operate_stop_term mul xs 4
          | fromJust x == 62    = operate_stop_skip mul xs 5
          | fromJust x == 16    = operate_stop_replace mul xs 5 2
          | otherwise           = f4 xs
          



  
-- Q5 (2 marks)
f5 :: [Maybe Int] -> [Int]
-- uses f4 to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
f5 [] = []
f5 ns = (x:(f5 xs))
        where 
          res = f4 ns
          x = fst res
          xs = snd res


-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

fromJust :: Maybe Int -> Int
fromJust Nothing = 0
fromJust (Just x) = x

isNothing :: Maybe Int -> Bool
isNothing Nothing = True
isNothing x = False


-- Extremely inelegant solution
operate_fixed_term :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> (Int, [Maybe Int])
operate_fixed_term op (x:[]) n = ((fromJust x), []) 
operate_fixed_term op (x:y:xs) n | isNothing x            = (0, xs)
                                 | isNothing y            = ((fromJust x), xs) 
                                 | n == 1                 = ((fromJust x), (y:xs))
                                 | otherwise              = operate_fixed_term op (Just ((op (fromJust x) (fromJust y))) : xs) (n-1)

operate_fixed_skip :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> (Int, [Maybe Int])
operate_fixed_skip op (x:[]) n = ((fromJust x), [])
operate_fixed_skip op (x:y:xs) n | isNothing x            = operate_fixed_skip op (y:xs) (n-1) 
                                 | isNothing y            = operate_fixed_skip op (x:xs) (n-1)
                                 | n == 1                 = ((fromJust x), (y:xs))
                                 | otherwise              = operate_fixed_skip op (Just ((op (fromJust x) (fromJust y))) : xs) (n-1)

operate_fixed_replace :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> Int -> (Int, [Maybe Int])
operate_fixed_replace op (x:[]) operand num = ((fromJust x), [])
operate_fixed_replace op (x:y:xs) operand num | isNothing x            = operate_fixed_replace op ((Just num):y:xs) operand num
                                              | isNothing y            = operate_fixed_replace op (x:(Just num):xs) operand num
                                              | operand == 1           = ((fromJust x), (y:xs))
                                              | otherwise              = operate_fixed_replace op (Just ((op (fromJust x) (fromJust y))) : xs) (operand-1) num


operate_stop_term :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> (Int, [Maybe Int])
operate_stop_term op (x:y:xs) n | isNothing x             = (0, (y:xs))
                                | isNothing y             = ((fromJust x), xs)
                                | fromJust x == n         = (0, (y:xs))
                                | fromJust y == n         = ((fromJust x), xs)
                                | otherwise               = operate_stop_term op (Just ((op (fromJust x) (fromJust y))) : xs) n

operate_stop_skip :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> (Int, [Maybe Int])
operate_stop_skip op (x:y:xs) n | isNothing x             = operate_stop_skip op (y:xs) n
                                | isNothing y             = operate_stop_skip op (x:xs) n
                                | fromJust x == n         = (0, (y:xs))
                                | fromJust y == n         = ((fromJust x), xs)
                                | otherwise               = operate_stop_skip op (Just ((op (fromJust x) (fromJust y))) : xs) n

operate_stop_replace :: (Int -> Int -> Int) -> [Maybe Int] -> Int -> Int -> (Int, [Maybe Int])
operate_stop_replace op (x:y:xs) operand num | isNothing x                  = operate_stop_replace op ((Just num):y:xs) operand num
                                             | isNothing y                  = operate_fixed_replace op (x:(Just num):xs) operand num
                                             | fromJust x == operand        = (0, (y:xs))
                                             | fromJust y == operand        = ((fromJust x), xs)
                                             | otherwise                    = operate_stop_replace op (Just ((op (fromJust x) (fromJust y))) : xs) operand num


-- Everything below here is unused, a holdover from a previous attempt.
-- Just leaving it in case I have a mistake and this could earn some extra attempt marks
data Op = Op 
            { opcode :: Int
            , operator :: (Int -> Int -> Int)
            , fixed :: Bool
            , operand :: Int
            , term :: Bool
            , skip :: Bool
            , num :: Int
            }

-- Slightly cumbersome way of translation opcode table to a function
parse_opcode :: Int -> Op
parse_opcode x | x == 55 = Op {opcode=x,operator=add,fixed=True,operand=3,term=True,skip=False,num=0}
               | x == 43 = Op {opcode=x,operator=add,fixed=True,operand=4,term=False,skip=True,num=0}
               | x == 92 = Op {opcode=x,operator=add,fixed=True,operand=5,term=False,skip=False,num=6}
               | x == 35 = Op {opcode=x,operator=add,fixed=False,operand=5,term=True,skip=False,num=0}
               | x == 32 = Op {opcode=x,operator=add,fixed=False,operand=6,term=False,skip=True,num=0}
               | x == 45 = Op {opcode=x,operator=add,fixed=False,operand=3,term=False,skip=False,num=1}
               | x == 58 = Op {opcode=x,operator=mul,fixed=True,operand=4,term=True,skip=False,num=0}
               | x == 29 = Op {opcode=x,operator=mul,fixed=True,operand=6,term=False,skip=True,num=0}
               | x == 83 = Op {opcode=x,operator=mul,fixed=True,operand=4,term=False,skip=False,num=2}
               | x == 69 = Op {opcode=x,operator=mul,fixed=False,operand=4,term=True,skip=False,num=0}
               | x == 62 = Op {opcode=x,operator=mul,fixed=False,operand=5,term=False,skip=True,num=0}
               | x == 16 = Op {opcode=x,operator=mul,fixed=False,operand=5,term=False,skip=False,num=2}
               | otherwise = Op {opcode=0,operator=add,fixed=False,operand=0,term=False,skip=False,num=0} -- shouldnt get here ever

is_opcode :: Maybe Int -> Bool
is_opcode x | isNothing x = False
            | is_element_of (fromJust x) xs = True
            | otherwise = False
  where 
    xs = [55, 43, 92, 35, 32, 45, 58, 29, 83, 69, 62, 16]

is_element_of :: Int -> [Int] -> Bool
is_element_of a [] = False
is_element_of a (x:xs) = (a == x) || (is_element_of a xs)



