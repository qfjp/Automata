module Control.Orphans where

import           Data.Naturals

instance Bounded a => Bounded (Maybe a) where
    minBound = Nothing
    maxBound = Just maxBound

instance (Bounded a, Enum a, Eq a) => Enum (Maybe a) where
    toEnum x
        | x == minBound = Nothing
        | x == maxBound = Just maxBound
        | otherwise = Just . toEnum $ x
    fromEnum Nothing = minBound
    fromEnum m@(Just x)
        | m == minBound = minBound
        | m == maxBound = maxBound
        | otherwise = fromEnum x
    succ Nothing = Just minBound
    succ (Just x) = Just . succ $ x
    pred Nothing = Nothing
    pred (Just x) =
        if x == minBound
            then Nothing
            else Just . pred $ x

instance (Eq a, Eq b, Bounded a, Bounded b, Enum a, Enum b) => Enum (a, b) where
    fromEnum (x, y) =
        fromEnum $ cantorPair (codeInt . fromEnum $ x) (codeInt . fromEnum $ y)
    toEnum x' =
        ( (toEnum . decodeInt . cantorFst $ x)
        , (toEnum . decodeInt . cantorSnd $ x))
      where
        x = toEnum x'
    succ (x, y)
        | x == maxBound && y == maxBound = (maxBound, maxBound)
        | x == maxBound = (minBound, succ y)
        | otherwise = (succ x, y)
    pred (x, y)
        | x == minBound && y == minBound = (minBound, minBound)
        | x == minBound = (maxBound, pred y)
        | otherwise = (pred x, y)

codeInt :: Int -> Nat
codeInt x = cantorPair (toEnum (2 + signum x)) (toEnum . abs $ x)

decodeInt :: Nat -> Int
decodeInt x = ((fromEnum . cantorFst $ x) - 2) * (fromEnum . cantorSnd $ x)

cantorPair :: Nat -> Nat -> Nat
cantorPair x y = (x + y + 1) * (x + y) `div` 2 + y

cantorFst :: Nat -> Nat
cantorFst z = cantorW z - cantorSnd z

cantorSnd :: Nat -> Nat
cantorSnd z = z - cantorT z

cantorW :: Nat -> Nat
cantorW z = ((sqrtInt (8 * z + 1)) - 1) `div` 2

cantorT :: Nat -> Nat
cantorT z =
    let w = cantorW z
     in (w * w + w) `div` 2

sqrtInt :: (Num a, Integral a) => a -> a
sqrtInt = fromInteger . floor . sqrt . fromIntegral
