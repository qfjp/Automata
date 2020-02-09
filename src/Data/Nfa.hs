module Data.Nfa
    ( Alphabetical(..)
    , Nfa
    , accepting
    , alphabet
    , alphSize
    , nfa
    , notInFinal
    , numStates
    , printNfa
    , stateList
    , startSet
    , transitions
    ) where

import           Control.Monad   (ap, liftM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

class Alphabetical a where
    globalAlphabet :: [Maybe a]

instance Alphabetical Char where
    globalAlphabet = Nothing : (map Just "abcdefghijklmnopqrstuvwxyz")

-- TODO: Change model to multi-start state Nfa
data RawNfa a s =
    RawNfa
        { _startSet :: Set s
        , _numStates :: Int
        , _alphSize :: Int
        , _accepting :: Set s
        , _transitions :: Map (s, Maybe a) (Set s)
        }
    deriving (Show, Eq)

map' :: (Ord a, Ord t) => (s -> t) -> RawNfa a s -> RawNfa a t
map' f (RawNfa starts nStates aSize accepts trans) =
    let mappedKeys = (M.mapKeys (\(state, char) -> (f state, char)) trans)
        mappedVals = M.map (\set -> S.map f set) mappedKeys
     in RawNfa
            { _startSet = (S.map f starts)
            , _numStates = nStates
            , _alphSize = aSize
            , _accepting = (S.map f accepts)
            , _transitions = mappedVals
            }

union' :: Ord s => RawNfa a s -> RawNfa a s -> RawNfa a s
union' n1 n2 =
    RawNfa
        { _startSet = starts
        , _numStates = nStates
        , _alphSize = aSize
        , _accepting = accepts
        , _transitions = trans
        }
  where
    starts = _startSet n1 `S.union` _startSet n2
    nStates = max (_numStates n1) (_numStates n2)
    aSize = max (_alphSize n1) (_alphSize n2)
    accepts = _accepting n1 `S.union` _accepting n2
    trans = undefined

data Nfa a s where
    Prim :: (Ord s) => RawNfa a s -> Nfa a s
    Return :: s -> Nfa a s
    Bind :: Nfa a s -> (s -> Nfa a t) -> Nfa a t

instance (Ord a, Show a, Enum s, Ord s, Show s) => Show (Nfa a s) where
    show = drop 3 . show . run

nfa :: Ord s
    => Set s
    -> Int
    -> Int
    -> Set s
    -> Map (s, Maybe a) (Set s)
    -> Nfa a s
nfa starts nStates aSize accepts trans =
    Prim $
    RawNfa
        { _startSet = starts
        , _numStates = nStates
        , _alphSize = aSize
        , _accepting = accepts
        , _transitions = trans
        }

empty' :: (Enum s, Ord s) => RawNfa a s
empty' = RawNfa (S.singleton (toEnum 0)) 1 0 S.empty M.empty

empty :: (Enum s, Ord s) => Nfa a s
empty = nfa (S.singleton (toEnum 0)) 1 0 S.empty M.empty

--run (Bind (Prim s) f)               = S.foldl' S.union S.empty (S.map (run . f) s)
--run (Bind (Return a) f)             = run (f a)
run :: (Ord a, Enum s, Ord s) => Nfa a s -> RawNfa a s
run (Prim s) = s
run (Return s) =
    RawNfa
        { _startSet = S.singleton s
        , _numStates = 2
        , _alphSize = 0
        , _accepting = S.empty
        , _transitions = (M.fromList [((toEnum 0, Nothing), S.singleton s)])
        }
--run (Bind (Prim s) f) = (map' (run . f) s)
run (Bind (Return a) f) = run (f a)

numStates :: (Ord a, Enum s, Ord s) => Nfa a s -> Int
numStates = _numStates . run

alphSize :: (Ord a, Enum s, Ord s) => Nfa a s -> Int
alphSize = _alphSize . run

accepting :: (Ord a, Enum s, Ord s) => Nfa a s -> Set s
accepting = _accepting . run

transitions :: (Ord a, Enum s, Ord s) => Nfa a s -> Map (s, Maybe a) (Set s)
transitions = _transitions . run

startSet :: (Ord a, Enum s, Ord s) => Nfa a s -> Set s
startSet = _startSet . run

instance Functor (Nfa a) where
    fmap = liftM

instance Applicative (Nfa a) where
    pure = return
    (<*>) = ap

instance Monad (Nfa a) where
    return = Return
    (>>=) = Bind

alphabet :: (Alphabetical a, Ord a, Enum s, Ord s) => Nfa a s -> [Maybe a]
alphabet n = take (1 + alphSize n) globalAlphabet

stateList :: (Ord a, Enum s, Ord s) => Nfa a s -> [s]
stateList n = map toEnum [0 .. numStates n - 1]

notInFinal :: (Ord a, Enum s, Ord s) => Nfa a s -> Set s
notInFinal n = (S.fromList (stateList n)) `S.difference` (accepting n)

sepList :: Show a => String -> [a] -> String
sepList sep l =
    case maybeList of
        "" -> ""
        xs -> init xs
  where
    maybeList = foldr (\x l -> (show x) ++ sep ++ l) [] l

printTransitions :: (Enum s, Ord s, Show s) => Nfa Char s -> [String]
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

printNfa :: (Enum s, Ord s, Show s) => Nfa Char s -> String
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
