module Data.Total
    ( Total(..)
    , ($-)
    , fromMap
    , fromList
    , toMap
    , toList
    ) where

import           Data.Map        (Map)
import qualified Data.Map        as M

import           Control.Orphans
import           Debug.Trace     (traceShow)

data Total a b =
    Total (a -> b)

instance (Bounded a, Eq a, Enum a, Ord a, Eq b) => Eq (Total a b) where
    x == y = toMap x == toMap y

instance (Bounded a, Eq a, Enum a, Ord a, Eq b, Ord b) => Ord (Total a b) where
    x `compare` y = toMap x `compare` toMap y

instance Functor (Total a) where
    fmap f (Total g) = Total (f . g)

instance Applicative (Total a) where
    (Total f) <*> (Total g) = Total (\x -> f x (g x))
    pure = Total . const

instance Monad (Total a) where
    return = pure
    (Total g) >>= f =
        Total $ \a ->
            let (Total h) = f (g a)
             in h a

instance (Bounded a, Enum a, Ord a, Show a, Show b) => Show (Total a b) where
    show f = show $ toMap f

($-) :: Total a b -> a -> b
tot $- a =
    let (Total f) = tot
     in f a

fromMap :: (Bounded a, Enum a, Ord a, Monoid b) => Map a b -> Total a b
fromMap map =
    Total $ \x ->
        case (map M.!? x) of
            Nothing -> mempty
            Just y -> y

fromList :: (Bounded a, Enum a, Ord a, Monoid b) => [(a, b)] -> Total a b
fromList = fromMap . M.fromList

toMap :: (Bounded a, Enum a, Ord a) => Total a b -> Map a b
toMap = M.fromList . toList

toList ::
       forall a b. (Bounded a, Enum a, Ord a)
    => Total a b
    -> [(a, b)]
toList tot =
    (minBound, tot $- minBound) :
    reverse
        [ (x, tot $- x)
        | x <- takeWhile (/= minBound) (iterate pred (maxBound :: a))
        ]
