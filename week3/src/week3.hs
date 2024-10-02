
--Part 1
data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

-- helper
append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)

instance Monad List where
    Nil >>= _ = Nil
    (Cons x xs) >>= f = append (f x) (xs >>= f)
    --return x = Cons x Nil (i think this is unnecessary because it is handled by pure above)

{-
PROVE: return a >>= f == f a

Starting with: return a >>= f
    = Cons a Nil >>= f                          (Definition of return)
    = append (f a) (Nil >>= f)                  (Definition of bind)
    = append (f a) Nil                          (definition of bind with a Nil)
    = f a                                       (definition of append)

PROVE: m >>= return == m

Starting with: m >>= return
    Case where m is Nil:
        Nil >>= return
        = Nil                                   (From definition of bind on a Nil, this is exactly m)
    Case where m is Cons x xs
        (Cons x xs) >>= return
        = append (return x) (xs >>= return)     (From definition of bind)
        = append (Cons x Nil) (xs >>= return)   (From definition of return x)
        = Cons x (append Nil (xs >>= return))   (From definition of append)
        = Cons x (xs >>= return)                (Definition of append on a Nil)
        = Cons x xs                             (We can recursively apply the earlier steps to xs >>= return 
                                                 we are left with exactly the list we started with)

PROVE:
(m >>= f) >>= g == m >>= (\x -> f x >>= g)

Starting with (m >>= f) >>= g
    Case where m is Nil:
        Left side:
        (Nil >>= f) >>= g
        Nil >>= g                               (Definition of bind on Nil)
        Nil                                     (Definition of bind on Nil)

        Right side:
        Nil >>= (\x -> f x >>= g)
        Nil                                     (Definition of bind on Nil)
    
    Case where m is (Cons x xs):
        Left side:
        ((Cons x xs) >>= f) >>= g
        = (append (f x) (xs >>= f)) >>= g
        = not sure where to go from here
-}

-----------------------------------------------------------------------------------------------------------

--Part 2
data Pair a b = P a b
    deriving (Show, Eq)

instance Functor Pair where
    fmap f (P a b) = P a (f b)


{-
    Not entirely sure how to prove the identity law, but i assume the identity function maps a value to itself, so
    fmap id (P a b)
    = fmap P a (id b)                   (where id b == b)
    so = fmap (P a b)

    Composition law:
    prove: fmap (f . g) (P a b) == (fmap f . fmap g) (P a b)

    Left side:
    fmap (f . g) (P a b)
    = P a ((f . g) b)
    = P a (f (g b))

    right side:
    (fmap f . fmap g) (P a b)
    = fmap f (fmap g (P a b))       (Definition of function composition)
    = fmap f (P a (g b))            (Definition of fmap)
    = P a (f (g b))                 (definition of fmap again)

    
-}

{-
instance Applicative Pair where
    pure x = P {something} b

so since pure has to be in the form 
pure :: b -> Pair a b
it is unclear how to actually define it since the Pair constructor requires two type arguments
I assume this also raises issues when defining <*> but i think showing we cant definitively define pure
is sufficient in showing that pair cant really be an instance of applicative
-}


main :: IO ()
main = do
    putStrLn "hello"

{-

src: https://en.wikibooks.org/wiki/Haskell/The_Functor_class
https://en.wikibooks.org/wiki/Haskell/Applicative_functors
-}