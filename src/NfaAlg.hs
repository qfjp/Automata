module NfaAlg
    ( epsilonRemoval
    , simulate'
    , simulate
    , updateTrans
    ) where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Nfa
import           Data.Set        (Set)
import qualified Data.Set        as S

fixWith :: (Eq a) => (a -> a) -> a -> a
fixWith f init =
    let x = f init
     in if x == init
            then x
            else fixWith f x

epsClosureState :: (Ord a, Ord s) => Nfa a s -> s -> Set s
epsClosureState n state = S.insert state $ transitions n M.! (state, Nothing)

epsClosureOnce :: (Ord a, Ord s) => Nfa a s -> Set s -> Set s
epsClosureOnce n = S.foldl' (\epsClos state -> epsClosureState n state) S.empty

epsClosure :: (Ord a, Ord s) => Nfa a s -> Set s -> Set s
epsClosure n = fixWith (\s -> s `S.union` epsClosureOnce n s)

readChar :: (Ord a, Ord s) => s -> Maybe a -> Nfa a s -> Set s
readChar state = readCharOnSet (S.singleton state)

readCharOnSet :: (Ord a, Ord s) => Set s -> Maybe a -> Nfa a s -> Set s
readCharOnSet states' char nfa =
    close $ S.foldl (\set state -> set `S.union` readOne state) S.empty states
  where
    readOne state = (transitions nfa) M.! (state, char)
    close = epsClosure nfa
    states = close states'

simulate :: (Ord a, Ord s) => [a] -> Nfa a s -> Bool
simulate s n = not . null $ simulate' s n `S.intersection` accepting n

simulate' :: (Ord a, Ord s) => [a] -> Nfa a s -> Set s
simulate' str nfa =
    foldl' (\set char -> readCharOnSet set (Just char) nfa) (startSet nfa) str

epsilonRemoval ::
       forall a s. (Ord a, Ord s)
    => Nfa a s
    -> Nfa a s
epsilonRemoval n =
    let newAccepts = fixWith (updateF n) (accepting n)
        newTrans = fixWith (updateTrans n) (transitions n)
        epsKeys = zip (S.toList $ stateSet n) (repeat Nothing)
        emptyEps =
            foldl'
                (\map key -> M.adjust (const S.empty) key map)
                newTrans
                epsKeys
     in nfa (startSet n) (stateSet n) (alphabet n) newAccepts emptyEps

pureTrans :: (Ord a, Ord s) => TransType a s -> s -> Maybe a -> Set s
pureTrans trans state char = trans M.! (state, char)

updateF :: (Ord a, Ord s) => Nfa a s -> Set s -> Set s
updateF nfa accepts =
    let qs =
            [ q
            | q <- S.toList $ notInFinal nfa
            , r <- S.toList $ pureTrans (transitions nfa) q Nothing
            , r `S.member` accepts
            ]
     in accepts `S.union`
        foldl' (\set state -> set `S.union` S.singleton state) S.empty qs

updateTrans :: (Ord a, Ord s) => Nfa a s -> TransType a s -> TransType a s
updateTrans nfa trans =
    let qASs =
            [ ((q, a), s)
            | q <- S.toList $ stateSet nfa
            , a <- S.toList $ alphabet nfa
            , r <- S.toList $ pureTrans trans q Nothing
            , s <- S.toList $ pureTrans trans r a
            , s `S.notMember` pureTrans trans q a
            ]
     in foldl'
            (\map (key, s) -> M.adjust (S.union (S.singleton s)) key map)
            trans
            qASs
