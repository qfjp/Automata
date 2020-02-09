module Data.Nfa
    ( Nfa(..)
    , alphabet
    , globalAlphabet
    , notInFinal
    , printNfa
    , stateList
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

globalAlphabet :: String
globalAlphabet = " abcdefghijklmnopqrstuvwxyz"

data Nfa =
    Nfa
        { numStates :: Int
        , alphSize :: Int
        , accepting :: Set Int
        , transitions :: Map (Int, Char) (Set Int)
        }
    deriving (Show, Eq)

alphabet :: Nfa -> String
alphabet n = take (1 + alphSize n) globalAlphabet

stateList :: Nfa -> [Int]
stateList n = [0 .. numStates n - 1]

notInFinal :: Nfa -> Set Int
notInFinal n = (S.fromList (stateList n)) `S.difference` (accepting n)

sepList :: Show a => String -> [a] -> String
sepList sep l =
    case maybeList of
        "" -> ""
        xs -> init xs
  where
    maybeList = foldr (\x l -> (show x) ++ sep ++ l) [] l

printTransitions :: Nfa -> [String]
printTransitions n =
    let keys = [(q, a) | q <- stateList n, a <- alphabet n]
        sets' = [(transitions n) M.! (q, a) | q <- stateList n, a <- alphabet n]
        sets = map ((transitions n) M.!) keys
        setLines = groupBy (1 + alphSize n) (map printSet sets)
     in map unwords setLines

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy size lst =
    let (nxtHead, nxtTail) = splitAt size lst
     in nxtHead : groupBy size nxtTail

printSet :: Show a => Set a -> String
printSet s = "{" ++ sepList "," (S.toList s) ++ "}"

printNfa :: Nfa -> String
printNfa n =
    "Number of states: " ++
    (show $ numStates n) ++
    "\n" ++
    "Alphabet size: " ++
    (show $ alphSize n) ++
    "\n" ++
    "Accepting states: " ++
    (sepList " " . S.toList . accepting $ n) ++
    "\n" ++ (unlines $ printTransitions n)
