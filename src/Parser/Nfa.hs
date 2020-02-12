module Parser.Nfa
    ( parseNfa
    , parseS
    , parseT
    ) where

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Control.Applicative     ((<|>))
import           Data.Char               (isSpace)
import           Text.Parser.Char        hiding (spaces)
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta.Parser
import           Text.Trifecta.Result

import           Data.FiniteNat
import           Data.Nfa

eof' :: Parser (Map (Maybe Char) (Set FiniteNat))
eof' = eof >> return M.empty

newline' :: Parser String
newline' = newline >> return "\n"

smallNat :: Parser FiniteNat
smallNat = read <$> some digit

smallInt :: Parser Int
smallInt = read <$> some digit

spaces :: Parser String
spaces = some (satisfy isSpaceNoNewline) <|> pure ""
  where
    isSpaceNoNewline c
        | c == '\n' = False
        | otherwise = isSpace c

commaSpace :: Parser String
commaSpace = do
    string ","
    s <- spaces
    return $ ',' : s

showErr :: ErrInfo -> String
showErr = show . _errDoc

parseS :: Parser a -> String -> Either String a
parseS p s =
    case parseString p mempty s of
        Success x -> Right x
        Failure x -> Left . showErr $ x

parseT :: Parser a -> Text -> Either String a
parseT p = parseS p . T.unpack

parseSet :: Ord a => Parser a -> Parser (Set a)
parseSet p = do
    string "{"
    result <- (S.fromList <$> sepBy p commaSpace)
    string "}"
    return result

parseNfa :: Parser (Nfa Char FiniteNat)
parseNfa = do
    text "Number of states: "
    numStates <- smallInt
    let states = S.fromList $ take numStates [minBound ..]
    newline'
    text "Alphabet size: "
    alphSize <- smallInt
    let alph = S.fromList $ Nothing : (map Just . take alphSize $ ['a' ..])
        transitionLine =
            M.fromList <$> zip (S.toList alph) <$>
            sepBy1 (parseSet smallNat) spaces
        transitionTableList = do
            indexedMaps <-
                zip [0 ..] <$> sepBy1 (transitionLine <|> eof') (newline)
            let stateMaps =
                    map (\(state, m) -> M.mapKeys (state, ) m) indexedMaps
            return $ foldr M.union M.empty stateMaps
    newline'
    text "Accepting states: "
    accepting <- S.fromList <$> sepBy smallNat spaces
    newline'
    transitions <- transitionTableList
    return $ nfa (S.singleton 0) states alph accepting transitions
  --where
    --transitionLine :: Parser (Map (Maybe Char) (Set FiniteNat))
    --transitionTableList :: Parser (TransType Char FiniteNat)
