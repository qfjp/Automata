module NfaAlg where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Nfa
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Debug.Trace     (trace)

fixWith :: (Eq a) => (a -> a) -> a -> a
fixWith f init =
    let x = f init
     in if x == init
            then x
            else fixWith f x

epsClosureState :: (Ord a, Enum s, Ord s) => Nfa a s -> s -> Set s
epsClosureState n state = S.insert state $ transitions n M.! (state, Nothing)

epsClosureOnce :: (Ord a, Enum s, Ord s) => Nfa a s -> Set s -> Set s
epsClosureOnce n = S.foldl' (\epsClos state -> epsClosureState n state) S.empty

epsClosure :: (Ord a, Enum s, Ord s) => Nfa a s -> Set s -> Set s
epsClosure n = fixWith (\s -> s `S.union` epsClosureOnce n s)

readChar :: (Ord a, Enum s, Ord s) => s -> Maybe a -> Nfa a s -> Set s
readChar state = readCharOnSet (S.singleton state)

readCharOnSet :: (Ord a, Enum s, Ord s) => Set s -> Maybe a -> Nfa a s -> Set s
readCharOnSet states' char nfa =
    close $ S.foldl (\set state -> set `S.union` readOne state) S.empty states
  where
    readOne state = (transitions nfa) M.! (state, char)
    close = epsClosure nfa
    states = close states'

simulate :: (Ord a, Enum s, Ord s) => [a] -> Nfa a s -> Bool
simulate s n = not . null $ simulate' s n `S.intersection` accepting n

simulate' :: (Ord a, Enum s, Ord s) => [a] -> Nfa a s -> Set s
simulate' str nfa =
    foldl' (\set char -> readCharOnSet set (Just char) nfa) (startSet nfa) str

epsilonRemoval :: (Alphabetical a, Ord a, Enum s, Ord s) => Nfa a s -> Nfa a s
epsilonRemoval n = do
    let oneEpsFinals -- Step 2
         =
            [ (q, r)
            | q <- S.toList $ notInFinal n
            , r <-
                  S.toList $
                  (transitions n M.! (q, Nothing)) `S.intersection` accepting n
            ]
    let newAccepts =
            (accepting n) `S.union` (S.fromList . map fst $ oneEpsFinals)
    let oneEpsStep -- Step 3
         =
            M.fromListWith
                (S.union)
                [ ((q, a), S.singleton s)
                | q <- stateList n
                , a <- alphabet n
                , r <- S.toList $ (transitions n) M.! (q, Nothing)
                , s <- S.toList $ (transitions n) M.! (r, a)
                , s `S.notMember` ((transitions n) M.! (q, a))
                ]
        transitionsAlt1 = M.unionWith S.union (transitions n) oneEpsStep
        transitionsAlt2 -- Step 4
         =
            foldr
                (\k m -> M.adjust (const S.empty) k m)
                transitionsAlt1
                (map (, Nothing) (stateList n))
        nextNfa =
            nfa
                (startSet n)
                (numStates n)
                (alphSize n)
                newAccepts
                transitionsAlt2
    nextNfa
