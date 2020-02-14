module Data.Nfa
    ( Nfa
    , TransType
    , union
    , empty
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

import           Data.Total                (Total)
import qualified Data.Total                as T

type TransType a s = Total (s, Maybe a) (Set s)

data RawNfa a s =
    RawNfa
        { _startSet :: Set s
        , _stateSet :: Set s
        , _alphabet :: Set (Maybe a)
        , _accepting :: Set s
        , _transitions :: TransType a s
        }

--    deriving (Show, Eq, Ord)
instance (Bounded a, Enum a, Ord a, Show a, Bounded s, Enum s, Ord s, Show s) =>
         Show (RawNfa a s) where
    show (RawNfa starts states alph acc trans) =
        "RawNfa { _startSet = " ++
        (show starts) ++
        ", _stateSet = " ++
        show states ++
        ", _alphabet = " ++
        (show alph) ++
        ", _accepting = " ++
        show acc ++ ", _transitions = " ++ show trans ++ "}"

instance (Bounded a, Bounded s, Eq a, Eq s, Enum a, Enum s, Ord a, Ord s) =>
         Eq (RawNfa a s) where
    x == y =
        (_startSet x == _startSet y) &&
        (_stateSet x == _stateSet y) &&
        (_alphabet x == _alphabet y) &&
        (_accepting x == _accepting y) && (_transitions x == _transitions y)

instance (Bounded a, Bounded s, Eq a, Eq s, Enum a, Enum s, Ord a, Ord s) =>
         Ord (RawNfa a s) where
    x `compare` y =
        (_startSet x, _stateSet x, _alphabet x, _accepting x, _transitions x) `compare`
        (_startSet y, _stateSet y, _alphabet y, _accepting y, _transitions y)

data Nfa a s where
    Prim :: (Ord s) => RawNfa a s -> Nfa a s
    Return :: s -> Nfa a s
    Bind :: Nfa a s -> (s -> Nfa a t) -> Nfa a t
    --Zero :: Nfa a s
    --Plus :: Nfa a s -> Nfa a s -> Nfa a s

instance ( Arbitrary a
         , Bounded a
         , Enum a
         , Ord a
         , Arbitrary s
         , Enum s
         , Bounded s
         , Ord s
         ) =>
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
                , _transitions = T.fromMap transitions
                }

instance (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) =>
         EqProp (Nfa a s) where
    x =-= y = property $ x == y

instance (Bounded a, Enum a, Ord a, Show a, Bounded s, Enum s, Ord s, Show s) =>
         Show (Nfa a s) where
    show = drop 3 . show . run

instance (Bounded a, Enum a, Eq a, Ord a, Bounded s, Enum s, Eq s, Ord s) =>
         Eq (Nfa a s) where
    x == y = (run x) == (run y)

instance (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) =>
         Ord (Nfa a s) where
    compare n1 n2 = compare (tuplefy n1) (tuplefy n2)
      where
        tuplefy n =
            (startSet n, stateSet n, alphabet n, accepting n, transitions n)

instance ( Arbitrary a
         , Bounded a
         , Enum a
         , Ord a
         , Arbitrary s
         , Bounded s
         , Enum s
         , Ord s
         ) =>
         Arbitrary (Nfa a s) where
    arbitrary = Prim <$> arbitrary

return' ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => s -> RawNfa a s
return' state =
    let states = S.singleton state
        trans =
            T.fromMap . M.fromList $
            [ ((state, char), S.empty)
            | state <- (S.toList states)
            , char <- [Nothing]
            ]
     in RawNfa states states S.empty states trans

alterfunc key map Nothing =
    case map M.!? key of
        Nothing -> Just S.empty
        x -> x
alterfunc key map (Just set) = Just $ set `S.union` (map M.! key)

innerExplode ::
       forall c s. (Ord c, Ord s)
    => Map (Set s, c) (Set s)
    -> Map (s, c) (Set s)
    -> (Set s, c)
    -> Map (s, c) (Set s)
innerExplode origMap map (keySet, char) =
    M.unionWith
        S.union
        map
        (S.foldl'
             (\map' key ->
                  M.alter (alterfunc (keySet, char) origMap) (key, char) map')
             M.empty
             keySet)

explode ::
       forall a t. (Ord a, Ord t)
    => Map (Set t, a) (Set t)
    -> Map (t, a) (Set t)
explode map = foldl' (innerExplode map) M.empty $ M.keys map

bind' ::
       forall a s t.
       ( Bounded a
       , Enum a
       , Ord a
       , Bounded s
       , Enum s
       , Ord s
       , Bounded t
       , Enum t
       , Ord t
       )
    => RawNfa a s
    -> (s -> RawNfa a t)
    -> RawNfa a t
oldNfa@(RawNfa starts states alphabet accepts trans) `bind'` f =
    RawNfa starts' states' alphabet accepts' (T.fromMap trans')
  where
    transMapVals :: Map (s, Maybe a) (RawNfa a t)
    transMapVals = M.map (S.foldl' union' empty' . S.map f) (T.toMap trans)
    transMapped :: Map (RawNfa a t, Maybe a) (RawNfa a t)
    transMapped = M.mapKeys (\(state, char) -> (f state, char)) transMapVals
    transStartVals :: Map (RawNfa a t, Maybe a) (Set t)
    transStartVals = M.map _startSet transMapped
    transStarts :: Map (Set t, Maybe a) (Set t)
    transStarts =
        M.mapKeys (\(nfa, char) -> (_startSet nfa, char)) transStartVals
    trans' = explode transStarts
    starts' :: Set t
    starts' =
        S.foldl'
            (\set nfa -> set `S.union` _startSet nfa)
            S.empty
            (S.map f starts)
    states' :: Set t
    states' =
        S.foldl
            (\set nfa -> set `S.union` _stateSet nfa)
            S.empty
            (S.map f states)
    accepts' :: Set t
    accepts' =
        S.foldl'
            (\set nfa -> set `S.union` _accepting nfa)
            S.empty
            (S.map f accepts)

union' ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s)
    => RawNfa a s
    -> RawNfa a s
    -> RawNfa a s
union' n1 n2 =
    RawNfa
        { _startSet = starts
        , _stateSet = states
        , _alphabet = alph
        , _accepting = accepts
        , _transitions = T.fromMap trans
        }
  where
    starts = _startSet n1 `S.union` _startSet n2
    states = _stateSet n1 `S.union` _stateSet n2
    alph = _alphabet n1 `S.union` _alphabet n2
    accepts = _accepting n1 `S.union` _accepting n2
    trans =
        M.unionWith
            S.union
            (T.toMap $ _transitions n1)
            (T.toMap $ _transitions n2)

union ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s)
    => Nfa a s
    -> Nfa a s
    -> Nfa a s
union x y = Prim $ union' (run x) (run y)

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

empty' :: (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => RawNfa a s
empty' = RawNfa S.empty S.empty S.empty S.empty (T.fromMap M.empty)

empty :: (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s
empty = Prim empty'

run :: (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s)
    => Nfa a s
    -> RawNfa a s
run (Prim s) = s
run (Return s) = return' s
--run (Bind (Prim s) f) = join' (fmap' (run . f) s)
--run (Bind (Prim s) f) = s `bind'` (run . f)
run (Bind (Return a) f) = run (f a)
run (Bind (Bind ma f) g) = run (Bind ma (\a -> Bind (f a) g))

stateSet ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
stateSet = _stateSet . run

alphabet ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s)
    => Nfa a s
    -> Set (Maybe a)
alphabet = _alphabet . run

alphSize ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
alphSize = (\x -> x - 1) . S.size . alphabet

accepting ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
accepting = _accepting . run

transitions ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s)
    => Nfa a s
    -> TransType a s
transitions = _transitions . run

startSet ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
startSet = _startSet . run

instance Functor (Nfa a) where
    fmap = liftM

instance Applicative (Nfa a) where
    pure = return
    (<*>) = ap

instance Monad (Nfa a) where
    return = Return
    (>>=) = Bind

stateList ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> [s]
stateList = S.toList . stateSet

numStates ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Int
numStates = S.size . stateSet

notInFinal ::
       (Bounded a, Enum a, Ord a, Bounded s, Enum s, Ord s) => Nfa a s -> Set s
notInFinal n = (S.fromList (stateList n)) `S.difference` (accepting n)

sepList :: Show a => String -> [a] -> String
sepList sep l =
    case maybeList of
        "" -> ""
        xs -> init xs
  where
    maybeList = foldr (\x l -> (show x) ++ sep ++ l) [] l

printTransitions ::
       (Bounded a, Enum a, Ord a, Show a, Bounded s, Enum s, Ord s, Show s)
    => Nfa a s
    -> [String]
printTransitions n =
    let keys = [(q, a) | q <- stateList n, a <- (S.toList . alphabet $ n)]
        sets' =
            [ (transitions n) T.$- (q, a)
            | q <- stateList n
            , a <- (S.toList . alphabet $ n)
            ]
        sets = map ((transitions n) T.$-) keys
        setLines = groupBy (1 + alphSize n) (map printSet sets)
        transLines =
            zipWith
                (\state transList -> ((show state) ++ ":") : transList)
                (stateList n)
                setLines
     --in map unwords transLines
     in map unwords setLines

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy size lst =
    let (nxtHead, nxtTail) = splitAt size lst
     in nxtHead : groupBy size nxtTail

printSet :: Show a => Set a -> String
printSet s = "{" ++ sepList "," (S.toList s) ++ "}"

printNfa ::
       (Bounded a, Enum a, Ord a, Show a, Bounded s, Enum s, Ord s, Show s)
    => Nfa a s
    -> String
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
