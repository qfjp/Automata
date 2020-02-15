module Data.LatinChar
    ( LatinChar
    , latChr
    ) where

import           Test.QuickCheck.Arbitrary (Arbitrary(..))

data LatinChar =
    LatinChar Char
    deriving (Eq, Ord)

instance Arbitrary LatinChar where
    arbitrary = latChr <$> arbitrary

instance Show LatinChar where
    show (LatinChar x) = show x

instance Bounded LatinChar where
    minBound = LatinChar 'a'
    maxBound = LatinChar 'z'

instance Enum LatinChar where
    fromEnum (LatinChar x) = fromEnum x
    toEnum = latChr . toEnum

latChr :: Char -> LatinChar
latChr a
    | a' < minBound = minBound
    | a' > maxBound = maxBound
    | otherwise = a'
  where
    a' = LatinChar a
