module Ex3 where

--required for all Qs:
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
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> CExpr -> Float
eval d (Value x)                              = x
eval d (Variable x)                           = fromJust (find x d)
eval d (DvdBy x y)                            = (eval d x) / (eval d y) 
eval d (Subtract x y)                         = (eval d x) - (eval d y)
eval d (Magnitude x) | z == 0                 = 0.0
                     | z < 0                  = z * (-1) 
                     | otherwise              = z
                     where z = eval d x
eval d (Not x) | (eval d x) == 0.0            = 1    --True
               | otherwise                    = 0.0  --False
eval d (Dfrnt x y) | (eval d x) == (eval d y) = 0.0  --False
                   | otherwise                = 1    --True
eval d (NotNull x) | (eval d x) == 0.0        = 0.0  --False
                   | otherwise                = 1    --True


-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> CExpr -> Maybe Float

meval d (Value x)                              = Just x

meval [] (Variable x)                          = Nothing

meval d (Variable x)                           = (find x d)

meval d (DvdBy x y)
 = let r = meval d x ; s = meval d y
    in case (r,s) of 
      (_, Just 0.0) -> Nothing
      (Just m, Just n) -> Just ((fromJust r) / (fromJust s))
      _      -> Nothing

meval d (Subtract x y)
  = let a = meval d x ; b = meval d y
    in case (a,b) of 
      (Just m, Just n) -> Just ((fromJust a) - (fromJust b))
      _                -> Nothing

meval d (Magnitude x) = absol (meval d x)

meval d (Not x) | (isNothing z)               = Nothing
                | (fromJust z) == 0.0         = Just 1
                | otherwise                   = Just 0
                where z = (meval d x)
      
meval d (Dfrnt x y) 
  = let a = meval d x ; b = meval d y
    in case (a,b) of
      (Just m, Just n) -> Just (eval d (Dfrnt (Value (fromJust a)) (Value (fromJust b))))
      _ -> Nothing

meval d (NotNull x) | z == Nothing = Nothing
                    | (fromJust z) == 0.0 = Just 0
                    | otherwise = Just 1
                    where z = (meval d x)


-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* CExpr datatype.
simp :: CExpr -> CExpr
simp (DvdBy a b) = (DvdBy (simp a) (simp b))
simp (Magnitude x) = (Magnitude (simp x))
simp (Not x) = (Not (simp x))
simp (Dfrnt x y) = (Dfrnt (simp x) (simp y))
simp (NotNull x) = (NotNull (simp x))
simp (Subtract x y) | b == (Value 0) = a
                    | a == b = Value 0
                    | otherwise = (Subtract a b)
                    where a = simp x ; b = simp y
simp a = a


-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

--These are taken from data.Maybe
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

fromJust :: Maybe a -> a
fromJust Nothing  = error "Data.Strict.Maybe.fromJust: Nothing"
fromJust (Just x) = x

isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False


absol :: Maybe Float -> Maybe Float
absol Nothing = Nothing
absol x | (fromJust x) == 0 = Just 0
        | (fromJust x) < 0 = Just ((fromJust x) * (-1))
        | otherwise = x

--
--mevalOp :: Dict -> (Float -> Float -> Float) -> Maybe Float
--mevalOp d op x y
--  = let r = meval d x ; s = meval d y
--    in case (r,s) of
--      (_, Just 0,0)      -> Nothing
--      (Just m, Just n)   -> Just (op m n)
--      _                  -> Nothing