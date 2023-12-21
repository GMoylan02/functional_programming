module Ex4 where

import System.IO

--required for Q1
data CExpr -- the expression datatype
  = Value Float -- floating-point value
  | Variable String -- variable/identifier name
  | DvdBy CExpr CExpr -- divide first by second
  | Subtract CExpr CExpr -- subtracts second from first
  | Magnitude CExpr -- absolute value
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not CExpr -- logical not
  | Dfrnt CExpr CExpr -- True if both are different
  | NotNull CExpr -- True if numeric value is non-zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: MonadFail m => String -> Dict -> m Float
find s [] = fail (s++" not found")
find s ((t,f):d)
  | s==t       =  return f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = ([Float],Bool)

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which always returns a value):
mdeval :: MonadFail m => Dict -> CExpr -> m Float
mdeval _ (Value x) = return x
mdeval d (Variable x) = (find x d)
mdeval d (DvdBy x y)
  = do b <- mdeval d y
       if b == 0.0
        then fail "Divide by zero"
        else do a <- mdeval d x
                return (a/b)
mdeval d (Subtract x y) = mdevalOP d (-) x y
mdeval d (Magnitude x) 
  = do b <- (mdeval d x)
       return (abs b)
mdeval d (Not x)
  = do b <- (mdeval d x)
       if b == 0.0
        then return 1
        else return 0
mdeval d (Dfrnt x y)
  = do a <- (mdeval d x)
       b <- (mdeval d y)
       if a == b
        then return 0
        else return 1
mdeval d (NotNull x)
  = do b <- (mdeval d x)
       if b == 0.0
        then return 0
        else return 1



-- Q2 (8 marks)
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = x `incsnd` (len z xs)
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = n + (sumup sbase ns)
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = n * (prod mbase ns)
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = xs ++ (cat pfx xss)

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldR z _ [] = z
foldR z op (x:xs) = x `op` foldR z op xs

-- We can gather the `z` and `opr` arguments into a tuple: (z,op)
-- which allows us to construct a call to foldR as:
dofold (z,op) = foldR z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

-- dofold lenTuple = len
lenTuple :: (Int,Int -> Int -> Int)
lenTuple = (0,(incsnd))

-- dofold sumupTuple = sumup
sumupTuple :: (Int,Int -> Int -> Int)
sumupTuple = (0,(+))

-- dofold prodTuple = prod
prodTuple :: (Int,Int -> Int -> Int)
prodTuple = (1,(*))

-- dofold catTuple = cat
catTuple :: ([Thing],[Thing] -> [Thing] -> [Thing])
catTuple = ([],(++))

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(+26),(sub 20),(+21),(*28),(sub 29),(*21),(sub 27),(sub 26),(25-),(+29),(*19),(*22)]

apply_ops :: Handle -> Handle -> [Integer -> Integer] -> IO ()
apply_ops input_handle output_handle (x:xs) = do
  eof <- hIsEOF input_handle
  if eof 
    then return ()
    else do
      s <- hGetLine input_handle
      let line = read s :: Integer
      hPutStrLn output_handle (show (x line))
      apply_ops input_handle output_handle (xs++[x])
apply_ops _ _ [] = return ()

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

mdevalOP d op x y
  = do a <- mdeval d x
       b <- mdeval d y
       return (a `op` b)
