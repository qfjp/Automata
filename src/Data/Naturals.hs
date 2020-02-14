-- | Module to model a state label
module Data.Naturals
    ( SmallNat
    , Nat
    ) where

import           GHC.Real

data Nat =
    Nat Int
    deriving (Eq, Ord)

instance Show Nat where
    show (Nat x) = show x

instance Bounded Nat where
    minBound = Nat 0
    maxBound = Nat maxBound

instance Enum Nat where
    toEnum x =
        if x < 0
            then Nat 0
            else Nat x
    fromEnum (Nat x) = x

instance Real Nat where
    toRational (Nat x) = (fromIntegral x) :% 1

instance Integral Nat where
    quotRem (Nat x) (Nat y) = (Nat $ div x y, Nat $ mod x y)
    divMod (Nat x) (Nat y) = (Nat $ div x y, Nat $ mod x y)
    toInteger (Nat x) = fromIntegral x

instance Num Nat where
    (Nat x) + (Nat y) =
        let result = x + y
         in Nat $
            if result > maxBound
                then maxBound
                else result
    (Nat x) - (Nat y) =
        let result = x - y
         in Nat $
            if result < minBound
                then minBound
                else result
    (Nat x) * (Nat y) =
        let result = x * y
         in Nat $
            if result > maxBound
                then maxBound
                else result
    abs = id
    signum = const minBound
    fromInteger x
        | x > max = Nat . fromIntegral $ max
        | x < min = Nat . fromIntegral $ min
        | otherwise = Nat . fromIntegral $ x
      where
        (Nat min') = minBound
        (Nat max') = maxBound
        min = fromIntegral min'
        max = fromIntegral max'

data SmallNat =
    SmallNat Int
    deriving (Eq, Ord)

instance Show SmallNat where
    show (SmallNat x) = show x

instance Bounded SmallNat where
    minBound = SmallNat 0
    maxBound = SmallNat 63

instance Enum SmallNat where
    toEnum = fromInteger . fromIntegral
    fromEnum (SmallNat x) =
        if x < 0
            then 0
            else x

instance Num SmallNat where
    (SmallNat x) + (SmallNat y) =
        let result = x + y
         in SmallNat $
            if result > maxBound
                then maxBound
                else result
    (SmallNat x) - (SmallNat y) =
        let result = x - y
         in SmallNat $
            if result < minBound
                then minBound
                else result
    (SmallNat x) * (SmallNat y) =
        let result = x * y
         in SmallNat $
            if result > maxBound
                then maxBound
                else result
    abs = id
    signum = const minBound
    fromInteger x
        | x > max = SmallNat . fromIntegral $ max
        | x < min = SmallNat . fromIntegral $ min
        | otherwise = SmallNat . fromIntegral $ x
      where
        (SmallNat min') = minBound
        (SmallNat max') = maxBound
        min = fromIntegral min'
        max = fromIntegral max'

instance Read SmallNat where
    readsPrec i = \s -> map (\(a, b) -> (SmallNat a, b)) (readsPrec i s)
