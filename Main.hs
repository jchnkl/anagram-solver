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

-- So for example, if your input string is "helloworld", you're going to first
-- call your recursive tester function with "", passing the remaining available
-- letters "helloworld" as a 2nd parameter. Function sees that "" isn't a word,
-- but child "h" does exist. So it calls itself with "h", and "elloworld".
-- Function sees that "h" isn't a word, but child "e" exists. So it calls itself
-- with "he" and "lloworld". Function sees that "e" is marked, so "he" is
-- a word, take note. Further, child "l" exists, so next call is "hel" with
-- "loworld". It will next find "hell", then "hello", then will have to back out
-- and probably next find "hollow", before backing all the way out to the empty
-- string again and then starting with "e" words next.

type Depth = Int
type Length = Int

-- | A `Word` is either the input for the anagram solver or the result
type Word = String

-- | This is distinctive from a `String`: we are working with list operations on
-- a list of `Char`s, e.g. L.elem and L.delete
type CharList = [Char]

-- | Just a set of valid words for fast checks with `HashSet.member`
type WordSet = HashSet Word

-- | The DAWG which contains every word from the dictionary
type WordGraph = DAWG Char () ()

type Label = String
type AnagramTable = HashMap Label [Word]
type LabelSet = HashSet Label
type CharSet = HashSet Char

data AnagramCtx = AnagramCtx
    { labelSet :: LabelSet
    , anagramTable :: AnagramTable
    , anagramGraph :: WordGraph
    }
    deriving (Eq, Show)

quickNub :: (Eq a, Hashable a) => [a] -> [a]
quickNub = S.toList . S.fromList

defaultDictionary :: FilePath
defaultDictionary = "/usr/share/dict/cracklib-small"

buildLabelSet :: [String] -> LabelSet
buildLabelSet = S.fromList . quickNub . map L.sort

getLabelSet :: IO LabelSet
getLabelSet = buildLabelSet . words <$> readFile defaultDictionary

buildWordSet :: [String] -> WordSet
buildWordSet = S.fromList

getWordSet :: IO WordSet
getWordSet = buildWordSet . words <$> readFile defaultDictionary

buildGraph :: [String] -> WordGraph
buildGraph = G.fromLang
-- buildGraph = G.fromList . flip zip (repeat ()) . quickNub . map L.sort
-- buildGraph = G.fromLang . quickNub  . map L.sort
-- buildGraph = G.fromLang -- . quickNub  . map L.sort

getGraph :: IO WordGraph
getGraph = fmap (buildGraph . words) (readFile defaultDictionary)

buildTable :: [Word] -> AnagramTable
buildTable = H.map D.toList . H.fromListWith D.append . map toTuple
    where toTuple w = (toLabel w, D.singleton w)

getTable :: IO AnagramTable
getTable = fmap (buildTable . words) (readFile defaultDictionary)

toLabel :: String -> Label
toLabel = L.sort . map C.toLower

lookupAnagrams :: Word -> AnagramTable -> [Word]
lookupAnagrams w = fromMaybe [] . H.lookup (toLabel w)

bruteForceSolver :: AnagramTable -> Word -> [Word]
bruteForceSolver t w = concatMap (flip lookupAnagrams t)
                                 (filter (not . null) . L.subsequences $ L.sort w)

main :: IO ()
main = do
    graph <- fmap (buildGraph . words) (readFile defaultDictionary)
    putStrLn (map fst $ G.edges $ G.submap "a" $ graph)

isWord :: Word -> WordSet -> Bool
isWord = S.member

isValidEdge :: CharList -> (Char, WordGraph) -> Bool
isValidEdge chars (c',_) = c' `elem` chars

internalSolver :: WordSet -> String -> WordSet -> String -> WordGraph -> WordSet -- [String]
internalSolver wset wort acc [] _
    | isWord wort wset = S.insert wort acc
    | otherwise   = acc

internalSolver wset wort acc part g
    | L.null validEdges && isWord wort wset = S.insert wort acc
    | L.null validEdges = acc
    | otherwise = S.unions $ map findAnagrams validEdges

    where

    validEdges :: [(Char, WordGraph)]
    validEdges = filter (isValidEdge part) (G.edges g)

    findAnagrams :: (Char, WordGraph) -> WordSet
    findAnagrams (c',g')
        | isWord wort wset = internalSolver wset (wort++[c']) (S.insert wort acc) (c' `L.delete` part) g'
        | otherwise          = internalSolver wset (wort++[c']) acc (c' `L.delete` part) g'
