-- | Module to model a state label
module Data.FiniteNat
    ( FiniteNat
    ) where

newtype FiniteNat =
    FiniteNat Int
    deriving (Eq, Ord)

instance Show FiniteNat where
    show (FiniteNat x) = show x

instance Bounded FiniteNat where
    minBound = FiniteNat 0
    maxBound = FiniteNat 63

instance Enum FiniteNat where
    toEnum = fromInteger . fromIntegral
    fromEnum (FiniteNat x) = x

instance Num FiniteNat where
    (FiniteNat x) + (FiniteNat y) =
        let result = x + y
         in FiniteNat $
            if result > maxBound
                then maxBound
                else result
    (FiniteNat x) - (FiniteNat y) =
        let result = x - y
         in FiniteNat $
            if result < minBound
                then minBound
                else result
    (FiniteNat x) * (FiniteNat y) =
        let result = x * y
         in FiniteNat $
            if result > maxBound
                then maxBound
                else result
    abs = id
    signum = const minBound
    fromInteger x
        | x > max = FiniteNat . fromIntegral $ max
        | x < min = FiniteNat . fromIntegral $ min
        | otherwise = FiniteNat . fromIntegral $ x
      where
        (FiniteNat min') = minBound
        (FiniteNat max') = maxBound
        min = fromIntegral min'
        max = fromIntegral max'

instance Read FiniteNat where
    readsPrec i = \s -> map (\(a, b) -> (FiniteNat a, b)) (readsPrec i s)
