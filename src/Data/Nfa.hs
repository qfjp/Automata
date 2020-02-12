module Data.Nfa
    ( Nfa
    , TransType
    , bind'
    , accepting
    , alphabet
    , alphSize
    , nfa
    , notInFinal
    , numStates
    , stateSet
    , printNfa
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

type TransType a s = Map (s, Maybe a) (Set s)

data RawNfa a s =
    RawNfa
        { _startSet :: Set s
        , _stateSet :: Set s
        , _alphabet :: Set (Maybe a)
        , _accepting :: Set s
        , _transitions :: TransType a s
        }
    deriving (Show, Eq, Ord)

data Nfa a s where
    Prim :: (Ord s) => RawNfa a s -> Nfa a s
    Return :: s -> Nfa a s
    Bind :: Nfa a s -> (s -> Nfa a t) -> Nfa a t
    --Zero :: Nfa a s
    --Plus :: Nfa a s -> Nfa a s -> Nfa a s

instance (Arbitrary a, Ord a, Arbitrary s, Bounded s, Enum s, Ord s) =>
         Arbitrary (RawNfa a s) where
    arbitrary = do
        let maxSize = 4
            minSize = 0
        numStates <- choose (0, 4)
        alphSize <- choose (0, 3)
        states <- take numStates <$> listOf arbitrary
        alphabet' <- take alphSize <$> listOf arbitrary
        let alphabet = Nothing : alphabet'
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
                , _stateSet = S.fromList states
                , _alphabet = S.fromList alphabet
                , _accepting = accepting
                , _transitions = transitions
                }

instance (Ord a, Bounded s, Enum s, Ord s) => EqProp (Nfa a s) where
    x =-= y = property $ x == y

instance (Ord a, Show a, Bounded s, Enum s, Ord s, Show s) =>
         Show (Nfa a s) where
    show = drop 3 . show . run

instance (Eq a, Ord a, Enum s, Eq s, Bounded s, Ord s) => Eq (Nfa a s) where
    x == y = (run x) == (run y)

instance (Arbitrary a, Ord a, Arbitrary s, Bounded s, Enum s, Ord s) =>
         Arbitrary (Nfa a s) where
    arbitrary = Prim <$> arbitrary

return' :: (Ord a, Bounded s, Enum s, Ord s) => s -> RawNfa a s
return' state =
    let states = S.singleton state
        trans =
            M.fromList
                [ ((state, char), S.empty)
                | state <- (S.toList states)
                , char <- [Nothing]
                ]
     in RawNfa S.empty states S.empty S.empty trans

bind' ::
       forall a s t. (Enum t, Ord a, Ord t)
    => RawNfa a s
    -> (s -> RawNfa a t)
    -> RawNfa a t
oldNfa@(RawNfa starts nStates aSize accepts trans) `bind'` f = undefined
  where
    mappedKeys :: Map (RawNfa a t, Maybe a) (Set s)
    mappedKeys = M.mapKeys (\(state, char) -> (f state, char)) trans
    innerTrans :: Map (RawNfa a t, Maybe a) (Set (RawNfa a t))
    innerTrans = M.map (S.map f) mappedKeys
    starts' :: Set t
    starts' =
        S.foldl'
            (\set nfa -> set `S.union` _startSet nfa)
            S.empty
            (S.map f starts)
    accepts' :: Set t
    accepts' =
        S.foldl'
            (\set nfa -> set `S.union` _startSet nfa)
            S.empty
            (S.map f starts)
    --join' (fmap' f s)

union' :: (Ord a, Ord s) => RawNfa a s -> RawNfa a s -> RawNfa a s
union' n1 n2 =
    RawNfa
        { _startSet = starts
        , _stateSet = states
        , _alphabet = alph
        , _accepting = accepts
        , _transitions = trans
        }
  where
    starts = _startSet n1 `S.union` _startSet n2
    states = _stateSet n1 `S.union` _stateSet n2
    alph = _alphabet n1 `S.union` _alphabet n2
    accepts = _accepting n1 `S.union` _accepting n2
    trans = M.unionWith S.union (_transitions n1) (_transitions n2)

nfa :: Ord s
    => Set s
    -> Set s
    -> Set (Maybe a)
    -> Set s
    -> TransType a s
    -> Nfa a s
nfa starts states alph accepts trans =
    Prim $
    RawNfa
        { _startSet = starts
        , _stateSet = states
        , _alphabet = alph
        , _accepting = accepts
        , _transitions = trans
        }

empty' :: (Ord s) => RawNfa a s
empty' = RawNfa S.empty S.empty S.empty S.empty M.empty

empty :: (Ord s) => Nfa a s
empty = Prim empty'

run :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> RawNfa a s
run (Prim s) = s
run (Return s) = return' s
--run (Bind (Prim s) f) = join' (fmap' (run . f) s)
run (Bind (Return a) f) = run (f a)
run (Bind (Bind ma f) g) = run (Bind ma (\a -> Bind (f a) g))

stateSet :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
stateSet = _stateSet . run

alphabet :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set (Maybe a)
alphabet = _alphabet . run

alphSize :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
alphSize = (\x -> x - 1) . S.size . alphabet

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

--alphabet ::
--       (Alphabetical a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> [Maybe a]
--alphabet n = take (1 + alphSize n) globalAlphabet
stateList :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> [s]
stateList n = map toEnum [0 .. (numStates n - 1)]

numStates :: (Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
numStates = S.size . stateSet

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
    let keys = [(q, a) | q <- stateList n, a <- (S.toList . alphabet $ n)]
        sets' =
            [ (transitions n) M.! (q, a)
            | q <- stateList n
            , a <- (S.toList . alphabet $ n)
            ]
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
    (show . alphSize $ n) ++
    "\n" ++
    "Accepting states: " ++
    (sepList " " . S.toList . accepting $ n) ++
    "\n" ++ (unlines $ printTransitions n)
