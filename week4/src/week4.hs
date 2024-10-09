{-
--Q1: Lists of strings
newtype Writer a = Writer ([String], a)

runWriter :: Writer a -> ([String], a)
runWriter (Writer x) = x

instance Functor Writer where
    fmap f (Writer (s, a)) = Writer (s, (f a))

instance Applicative Writer where
    pure = return
    (Writer (l1, f)) <*> (Writer (l2, b)) = Writer (l1 ++ l2 , (f b))

instance Monad Writer where
    return a = Writer ([], a)
    (Writer (l1, x)) >>= f = 
        let Writer (l2, y) = f x
        in Writer (l1 ++ l2, y)

tell :: String -> Writer ()
tell s = Writer ([s], ())


example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 2"
  tell "entry 3"
  return (1 + 1)

-}

--Q2 part 1: generalised for lists of any type
newtype Writer w a = Writer ([w], a)

runWriter :: Writer w a -> ([w], a)
runWriter (Writer x) = x

instance Functor (Writer w) where
    fmap f (Writer (s, a)) = Writer (s, (f a))

instance Applicative (Writer w) where
    pure = return
    (Writer (l1, f)) <*> (Writer (l2, b)) = Writer (l1 ++ l2, (f b))

instance Monad (Writer w) where
    return a = Writer ([], a)
    (Writer (l1, x)) >>= f = 
        let Writer (l2, y) = f x
        in Writer (l1 ++ l2, y)

tell :: w -> Writer w ()
tell s = Writer ([s], ())

example :: Writer Int Int
example = do
  tell 6
  tell 4
  tell 4536
  return (1 + 1)


main :: IO ()
main = do
    print $ runWriter example

{-
    The current implementation doesn't really work if we generalise the monad and try letting the log be of any type. 
    This is because there is no sensible way to define how we accumulate or concatenate logs in general, given they
    can be of any type. Or even suppose we can come up with a sensible implenentation for like
    all the types we can think of (like addition on ints, strcat on strings etc), I do not know how this could be
    implemented.
-}
