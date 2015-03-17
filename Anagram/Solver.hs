{-# LANGUAGE CPP #-}

module Anagram.Solver where

import qualified Data.List as L

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Data.DAWG.Static (DAWG)
import qualified Data.DAWG.Static as G

#ifdef ANAGRAM_SOLVER_TEST_CASES
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)

import qualified Data.Char as C
import qualified Data.DList as D

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
-- #ifdef ANAGRAM_SOLVER_TEST_CASES
#endif

-- | A `Word` is either the input for the anagram solver or the result
type Word = String

-- | This is distinctive from a `String`: we are working with list operations on
-- a list of `Char`s, e.g. L.elem and L.delete
type CharList = [Char]

-- | Just a set of valid words for fast checks with `HashSet.member`
type WordSet = HashSet Word

-- | The DAWG which contains every word from the dictionary
type WordGraph = DAWG Char () ()

buildWordSet :: [String] -> WordSet
buildWordSet = S.fromList

buildGraph :: [String] -> WordGraph
buildGraph = G.fromLang

-- | Given a `Word` and a `WordSet` quickly check if this is a valid word from
-- the dictionary. Should be O(1) and therefor fast than the lookup in the graph
isWord :: Word -> WordSet -> Bool
isWord = S.member

-- | Check if a given graph edge is a valid candidate. For this, the edge symbol
-- must be contained within a given `CharList`.
isValidEdge :: CharList -> (Char, WordGraph) -> Bool
isValidEdge chars (c',_) = c' `elem` chars

-- | Filter all valid edges based on their symbol value. See `isValidEdge`.
validEdges :: CharList -> WordGraph -> [(Char, WordGraph)]
validEdges chars = filter (isValidEdge chars) . G.edges

-- | Find all anagrams for a given input word
internalSolver :: WordSet   -- ^ `WordSet` for `isWord`
               -> WordGraph -- ^ The `DAWG` containing all valid words from the dictionary
               -> ShowS     -- ^ Partial word accumulator for recursion. Initially empty: `showString ""`
                            --   TODO: Replace `CharList` with an unordered multiset
               -> CharList  -- ^ `CharList` of valid symbols. Initially equal to the input word
               -> WordSet   -- ^ Result `WordSet` accumulator for recursion. Initially empty
               -> WordSet   -- ^ All found anagrams
internalSolver wset graph word chars result
    | exit      = result'
    | otherwise = S.unions $ map solve edges
    where
    exit              = L.null edges || L.null chars
    edges             = validEdges chars graph
    word' c'          = word . showString [c']
    chars' c'         = c' `L.delete` chars
    result'           = if isWord (word "") wset then S.insert (word "") result else result
    solve (c',graph') = internalSolver wset graph' (word' c') (chars' c') result'

solver :: WordSet -> WordGraph -> Word -> [Word]
solver ws g input = S.toList $ internalSolver ws g (showString "") input S.empty

#ifdef ANAGRAM_SOLVER_TEST_CASES
-- | A `Tag` is a sorted `Word`, used as Key for the `WordTable`
type Tag = String

-- | A `HashMap` which holds a list for all similar `Word`s
-- similar means: `sort word1 == sort word2`
type WordTable = HashMap Tag [Word]
-- #ifdef ANAGRAM_SOLVER_TEST_CASES

defaultDictionary :: FilePath
defaultDictionary = "/usr/share/dict/cracklib-small"

readDictionary :: IO [String]
readDictionary = words <$> readFile defaultDictionary

getWordSet :: IO WordSet
getWordSet = buildWordSet <$> readDictionary

getWordGraph :: IO WordGraph
getWordGraph = buildGraph <$> readDictionary

buildTable :: [Word] -> WordTable
buildTable = H.map D.toList . H.fromListWith D.append . map toTuple
    where toTuple w = (toTag w, D.singleton w)

getWordTable :: IO WordTable
getWordTable = buildTable <$> readDictionary

-- | Faster than `Data.List.nub` at the expense of O(n) memory
quickNub :: (Eq a, Hashable a) => [a] -> [a]
quickNub = S.toList . S.fromList

toTag :: String -> Tag
toTag = L.sort . map C.toLower

lookupAnagrams :: Word -> WordTable -> [Word]
lookupAnagrams w = fromMaybe [] . H.lookup (toTag w)

-- | Naive solver: build all possible combinations for a string with
-- `Data.List.subsequences` and look them up in a hashtable. For largish
-- `String`s (somewhere above 20 characters) `Data.List.subsequences` takes an
-- eternity.
naiveSolver :: WordTable -> Word -> [Word]
naiveSolver t w = concatMap (flip lookupAnagrams t) sequences
    where sequences = filter (not . null) . L.subsequences $ L.sort w

-- | Some simple test cases, adapt to your liking
testCases :: [String]
testCases = ["zynismn","helloworld","curiouscaseofbenjaminbutton"]

-- | Find all anagrams for `testCases` with `naiveSolver` and `solver` and
-- compare the result
runTests :: IO ()
runTests = do
    words <- getWordSet
    graph <- getWordGraph
    table <- getWordTable
    forM_ testCases $ \test -> do
        let t1 = L.sort . quickNub $ naiveSolver table test
            t2 = L.sort $ solver words graph test
        if t1 == t2
            then putStrLn $ "Test case \"" ++ test ++ "\" was successful"
            else putStrLn $ "Test case \"" ++ test ++ "\" failed"

-- #ifdef ANAGRAM_SOLVER_TEST_CASES
#endif
