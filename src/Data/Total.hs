module Data.Total
    ( Total(..)
    , ($-)
    , fromMap
    , fromList
    , toMap
    , toList
    ) where

import           Data.List   (foldl')
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Set    (Set)
import qualified Data.Set    as S

import           Debug.Trace (traceShow)

data Total a b =
    Total
        { domain :: Set a
        , func :: (a -> b)
        }

instance (Eq a, Ord a, Eq b) => Eq (Total a b) where
    x == y = toMap x == toMap y

instance (Eq a, Ord a, Eq b, Ord b) => Ord (Total a b) where
    x `compare` y = toMap x `compare` toMap y

instance Functor (Total a) where
    fmap f (Total domain g) = Total domain (f . g)

instance Ord a => Applicative (Total a) where
    (Total domain1 f) <*> (Total domain2 g) =
        Total (domain1 `S.union` domain2) (f <*> g)
    pure x = Total (S.fromList [undefined]) (const x)

instance Ord a => Monad (Total a) where
    return = pure
    (Total dom g) >>= f =
        let origCoDomain = map g (S.toList dom)
            newDom = foldl' S.union S.empty (map domain (map f origCoDomain))
         in Total
                newDom
                (\a ->
                     let (Total _ h) = f (g a)
                      in h a)

instance (Ord a, Show a, Show b) => Show (Total a b) where
    show f = show $ toMap f

($-) :: Total a b -> a -> b
tot $- a =
    let (Total _ f) = tot
     in f a

fromMap :: (Ord a) => Map a b -> Total a b
fromMap map = Total (S.fromList $ M.keys map) $ \x -> map M.! x

fromList :: (Ord a, Monoid b) => [(a, b)] -> Total a b
fromList = fromMap . M.fromList

toMap :: (Ord a) => Total a b -> Map a b
toMap = M.fromList . toList

toList :: Ord a => Total a b -> [(a, b)]
toList tot@(Total domain f) = map (\x -> (x, f x)) $ S.toList domain
