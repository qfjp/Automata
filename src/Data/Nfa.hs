module Data.Nfa
    ( Alphabetical(..)
    , Nfa
    , TransType
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

import           Control.Monad             (ap, liftM)
import           Data.Foldable             (fold)
import           Data.List                 (foldl', (\\))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Test.QuickCheck           (property)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Checkers  (EqProp(..))
import           Test.QuickCheck.Gen       (choose, listOf, sublistOf)

import           Debug.Trace               (traceShow, traceShowId)

class Alphabetical a where
    globalAlphabet :: [Maybe a]

instance Alphabetical Char where
    globalAlphabet = Nothing : (map Just "abcdefghijklmnopqrstuvwxyz")

type TransType a s = Map (s, Maybe a) (Set s)

data RawNfa a s =
    RawNfa
        { _startSet :: Set s
        , _numStates :: Int
        , _alphSize :: Int
        , _accepting :: Set s
        , _transitions :: TransType a s
        }
    deriving (Show, Eq, Ord)

instance (Alphabetical a, Arbitrary a, Ord a, Arbitrary s, Enum s, Ord s) =>
         Arbitrary (RawNfa a s) where
    arbitrary = do
        let maxSize = 4
            minSize = 0
        numStates <- arbitrary
        alphSize <- choose (0, 3)
        let states = map toEnum [0 .. numStates]
            alphabet = take (1 + alphSize) globalAlphabet :: [Maybe a]
        startSet <- S.fromList <$> sublistOf states
        accepting <- S.fromList <$> sublistOf states
        let transitionKeys =
                M.fromList $
                [((state, char), states) | state <- states, char <- alphabet]
        transitions <-
            M.traverseWithKey
                (\(state, char) states ->
                     S.fromList <$> sublistOf (states \\ [state]))
                transitionKeys
        return
            RawNfa
                { _startSet = startSet
                , _numStates = numStates
                , _alphSize = alphSize
                , _accepting = accepting
                , _transitions = transitions
                }

instance (Ord a, Bounded s, Enum s, Ord s) => EqProp (Nfa a s) where
    x =-= y = property $ x == y

join' ::
       forall a s. (Ord a, Ord s)
    => RawNfa a (RawNfa a s)
    -> RawNfa a s
join' n =
    RawNfa
        { _startSet = starts
        , _numStates = numStates
        , _alphSize = alphSize
        , _accepting = accepts
        , _transitions = trans
        }
  where
    nfas :: [RawNfa a s]
    nfas =
        reverse $
        M.foldlWithKey
            (\nfaList (nfa, _) _ ->
                 if nfa `elem` nfaList -- TODO: SPEED
                     then nfaList
                     else nfa : nfaList)
            []
            (_transitions n)
    starts = fold . map _startSet $ nfas
    numStates = undefined
    alphSizeInner = foldl' max 0 . map _alphSize $ nfas
    alphSize = max (_alphSize n) alphSizeInner
    accepts = fold . map _accepting $ nfas
    transInners = map _transitions nfas
    trans = foldl' (M.unionWith S.union) M.empty transInners

fmap' :: (Ord a, Ord t) => (s -> t) -> RawNfa a s -> RawNfa a t
fmap' f (RawNfa starts numS aSize accepts trans) =
    let mappedKeys = (M.mapKeys (\(state, char) -> (f state, char)) trans)
        mappedVals = M.map (\set -> S.map f set) mappedKeys
     in RawNfa
            { _startSet = (S.map f starts)
            , _numStates = numS
            , _alphSize = aSize
            , _accepting = (S.map f accepts)
            , _transitions = mappedVals
            }

union' :: (Ord a, Ord s) => RawNfa a s -> RawNfa a s -> RawNfa a s
union' n1 n2 =
    RawNfa
        { _startSet = starts
        , _numStates = numStates
        , _alphSize = aSize
        , _accepting = accepts
        , _transitions = trans
        }
  where
    starts = _startSet n1 `S.union` _startSet n2
    numStates = max (_numStates n1) (_numStates n2)
    aSize = max (_alphSize n1) (_alphSize n2)
    accepts = _accepting n1 `S.union` _accepting n2
    trans = M.unionWith S.union (_transitions n1) (_transitions n2)

data Nfa a s where
    Prim :: (Ord s) => RawNfa a s -> Nfa a s
    Return :: s -> Nfa a s
    Bind :: Nfa a s -> (s -> Nfa a t) -> Nfa a t
    --Zero :: Nfa a s
    --Plus :: Nfa a s -> Nfa a s -> Nfa a s

instance (Ord a, Show a, Bounded s, Enum s, Ord s, Show s) =>
         Show (Nfa a s) where
    show = drop 3 . show . run

instance (Eq a, Ord a, Enum s, Eq s, Bounded s, Ord s) => Eq (Nfa a s) where
    x == y = (run x) == (run y)

instance (Alphabetical a, Arbitrary a, Ord a, Arbitrary s, Enum s, Ord s) =>
         Arbitrary (Nfa a s) where
    arbitrary = Prim <$> arbitrary

nfa :: Ord s => Set s -> Int -> Int -> Set s -> TransType a s -> Nfa a s
nfa starts numStates aSize accepts trans =
    Prim $
    RawNfa
        { _startSet = starts
        , _numStates = numStates
        , _alphSize = aSize
        , _accepting = accepts
        , _transitions = trans
        }

empty' :: (Ord s) => RawNfa a s
empty' = RawNfa S.empty 0 0 S.empty M.empty

empty :: (Ord s) => Nfa a s
empty = Prim empty'

run :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> RawNfa a s
run (Prim s) = s
run (Return s) = return' s
run (Bind (Prim s) f) = join' (fmap' (run . f) s)
run (Bind (Return a) f) = run (f a)
run (Bind (Bind ma f) g) = run (Bind ma (\a -> Bind (f a) g))

return' :: (Ord a, Bounded s, Enum s, Ord s) => s -> RawNfa a s
return' state =
    let single = S.singleton state
        states = [minBound .. state]
     in RawNfa
            { _startSet = single
            , _numStates = length states
            , _alphSize = 0
            , _accepting = S.empty
            , _transitions =
                  foldl'
                      (\map key -> M.insert (key, Nothing) S.empty map)
                      M.empty
                      states
            }

numStates :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
numStates = _numStates . run

alphSize :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
alphSize = _alphSize . run

accepting :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
accepting = _accepting . run

transitions :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> TransType a s
transitions = _transitions . run

startSet :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
startSet = _startSet . run

instance Functor (Nfa a) where
    fmap = liftM

instance Applicative (Nfa a) where
    pure = return
    (<*>) = ap

instance Monad (Nfa a) where
    return = Return
    (>>=) = Bind

alphabet ::
       (Alphabetical a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> [Maybe a]
alphabet n = take (1 + alphSize n) globalAlphabet

stateList :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> [s]
stateList n = [minBound .. toEnum (numStates n - 1)]

stateSet :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
stateSet = S.fromList . stateList

notInFinal :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
notInFinal n = (S.fromList (stateList n)) `S.difference` (accepting n)

sepList :: Show a => String -> [a] -> String
sepList sep l =
    case maybeList of
        "" -> ""
        xs -> init xs
  where
    maybeList = foldr (\x l -> (show x) ++ sep ++ l) [] l

printTransitions :: (Bounded s, Enum s, Ord s, Show s) => Nfa Char s -> [String]
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

printNfa :: (Bounded s, Enum s, Ord s, Show s) => Nfa Char s -> String
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
