module NfaAlg where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Nfa
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Debug.Trace     (trace)

readCharEps :: (Ord a, Ord s) => (s, Maybe a) -> Nfa a s -> Set s
readCharEps (state, char) nfa =
    S.fromList $ do
        states <- state : (S.toList $ (transitions nfa) M.! (state, Nothing))
        S.toList $ (transitions nfa) M.! (states, char)

readChar :: (Ord a, Ord s) => (s, Maybe a) -> Nfa a s -> Set s
readChar (state, char) nfa = (transitions nfa) M.! (state, char)

simulate :: (Ord a, Enum s, Ord s) => [a] -> Nfa a s -> Bool
simulate s n = not . null $ simulate' s n `S.intersection` accepting n

simulate' :: (Ord a, Enum s, Ord s) => [a] -> Nfa a s -> Set s
simulate' s = S.fromList . simulateHelp (toEnum 0, s)

simulateHelp :: (Ord a, Ord s) => (s, [a]) -> Nfa a s -> [s]
simulateHelp (state, []) nfa = do
    state : (S.toList $ readCharEps (state, Nothing) nfa)
simulateHelp (state, c:cs) nfa = do
    nextState <- S.toList $ readCharEps (state, Just c) nfa
    simulateHelp (nextState, cs) nfa

epsilonRemoval :: (Alphabetical a, Ord a, Enum s, Ord s) => Nfa a s -> Nfa a s
epsilonRemoval n = do
    let oneEpsFinals -- Step 2
         =
            [ (q, r)
            | q <- S.toList $ notInFinal n
            , r <-
                  S.toList $
                  (readChar (q, Nothing) n) `S.intersection` accepting n
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
                , r <- S.toList $ readChar (q, Nothing) n
                , s <- S.toList $ readChar (r, a) n
                , s `S.notMember` (readChar (q, a) n)
                ]
        transitionsAlt1 = M.unionWith S.union (transitions n) oneEpsStep
        transitionsAlt2 -- Step 4
         =
            foldr
                (\k m -> M.adjust (const S.empty) k m)
                transitionsAlt1
                (map (, Nothing) (stateList n))
        nextNfa = Nfa (numStates n) (alphSize n) newAccepts transitionsAlt2
    nextNfa
