module Main where

import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text, unpack)
import           NfaAlg
import           Parser.Nfa
import           System.Environment   (getArgs, getProgName)
import           Text.RawString.QQ
import           Text.Trifecta.Parser (parseFromFile)

import           Data.Nfa

testNfa :: Text
testNfa =
    [r|Number of states: 8
Alphabet size: 2
Accepting states: 1 4
{1,4} {}  {}
{}    {2} {}
{}    {}  {3}
{1}   {}  {}
{}    {5} {}
{}    {}  {6}
{}    {7} {}
{4}   {}  {}|]

printHelp :: IO ()
printHelp = do
    progName <- getProgName
    putStrLn ("USAGE: " ++ progName ++ " [MODE] [FILENAME]")
    putStrLn "    -S          simulation mode"
    putStrLn "    -C          conversion mode"
    putStrLn "    -h, --help  display this help and exit"

conversionEngine :: String -> IO ()
conversionEngine fpath = do
    parseFromFile parseNfa fpath >>= \maybNfa ->
        case maybNfa of
            Nothing -> return ()
            Just nfa -> do
                putStrLn . init . printNfa . epsilonRemoval $ nfa

simulationEngine :: String -> IO ()
simulationEngine fpath = do
    parseFromFile parseNfa fpath >>= \maybNfa ->
        case maybNfa of
            Nothing -> return ()
            Just nfa -> do
                strings <- lines <$> getContents
                let results =
                        map
                            (\s ->
                                 if simulate s nfa
                                     then "accept"
                                     else "reject")
                            strings
                mapM_ putStrLn results

chooseEngine :: String -> String -> IO ()
chooseEngine switch
    | switch == "-S" = simulationEngine
    | switch == "-C" = conversionEngine
    | otherwise = const printHelp

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 ->
            let [switch, fpath] = args
             in chooseEngine switch fpath
        _ -> printHelp
    case parseT parseNfa testNfa of
        Left x -> putStrLn x
        Right _ -> return ()
