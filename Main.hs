import Control.Applicative
import Data.Maybe
import Data.DAWG.Static (DAWG)
import qualified Data.DAWG.Static as G
import qualified Data.List as L

import qualified Data.DList as D
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import qualified Data.Char as C

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import qualified Data.HashSet as S

import Debug.Trace

-- | A `Word` is either the input for the anagram solver or the result
type Word = String

-- | This is distinctive from a `String`: we are working with list operations on
-- a list of `Char`s, e.g. L.elem and L.delete
type CharList = [Char]

-- | Just a set of valid words for fast checks with `HashSet.member`
type WordSet = HashSet Word

-- | The DAWG which contains every word from the dictionary
type WordGraph = DAWG Char () ()

type Tag = String
type AnagramTable = HashMap Tag [Word]

quickNub :: (Eq a, Hashable a) => [a] -> [a]
quickNub = S.toList . S.fromList

defaultDictionary :: FilePath
defaultDictionary = "/usr/share/dict/cracklib-small"

buildWordSet :: [String] -> WordSet
buildWordSet = S.fromList

getWordSet :: IO WordSet
getWordSet = buildWordSet . words <$> readFile defaultDictionary

buildGraph :: [String] -> WordGraph
buildGraph = G.fromLang

getGraph :: IO WordGraph
getGraph = fmap (buildGraph . words) (readFile defaultDictionary)

buildTable :: [Word] -> AnagramTable
buildTable = H.map D.toList . H.fromListWith D.append . map toTuple
    where toTuple w = (toTag w, D.singleton w)

getTable :: IO AnagramTable
getTable = fmap (buildTable . words) (readFile defaultDictionary)

toTag :: String -> Tag
toTag = L.sort . map C.toLower

lookupAnagrams :: Word -> AnagramTable -> [Word]
lookupAnagrams w = fromMaybe [] . H.lookup (toTag w)

bruteForceSolver :: AnagramTable -> Word -> [Word]
bruteForceSolver t w = concatMap (flip lookupAnagrams t)
                                 (filter (not . null) . L.subsequences $ L.sort w)

main :: IO ()
main = do
    graph <- fmap (buildGraph . words) (readFile defaultDictionary)
    putStrLn (map fst $ G.edges $ G.submap "a" $ graph)

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
