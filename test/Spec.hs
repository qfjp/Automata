import           Data.Nfa
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Classes

main :: IO ()
main = hspec $ nfaSpec

nfaSpec :: SpecWith ()
nfaSpec = do
    describe "Nfa Type Classes" $ do
        testBatch . functor $ (undefined :: Nfa Char (Int, Char, Int))
