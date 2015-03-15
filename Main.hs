import Control.Applicative
import Data.Maybe
import Data.DAWG.Static
import qualified Data.DAWG.Static as Dawg
import qualified Data.List as L

import Debug.Trace


type Word = String
type AnagramGraph = DAWG Char () ()

buildGraph :: [String] -> AnagramGraph
buildGraph = Dawg.fromList . flip zip (repeat ())

defaultDictionary :: FilePath
defaultDictionary = "/usr/share/dict/cracklib-small"

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

-- findAnagrams :: String -> [String]
-- findAnagrams = findAna
--     where
--     fa :: AnagramGraph -> String -> String -> [String] -> [String]
--     fa graph part rest 

    -- edges :: Enum a => DAWG a b c -> [(a, DAWG a b c)]

getGraph = fmap (buildGraph . words) (readFile defaultDictionary)

-- Dawg.submap l graph
--
-- hello
--
-- hellow



-- solve :: AnagramGraph -> Word -> [Word]

-- solve cws _ _ words _ [] = words -- (cws, words)
-- solve cws graph subgraph words word (c:cs)
--     | isWord curword graph = case isEndOfWord curword graph of
--         True -> solve (curword:cws) graph graph (curword : words) curword cs
--         False -> solve (curword:cws) graph (Dawg.submap curword subgraph) (curword : words) curword cs
--
--     -- solve (curword:cws) graph (Dawg.submap curword subgraph) (curword : words) curword cs
--
--     | otherwise            = case isEndOfWord curword graph of
--         True -> solve (curword:cws) graph graph words curword cs
--         False -> solve (curword:cws) graph (Dawg.submap curword subgraph) words curword cs
--     where curword = word ++ [c]


main :: IO ()
main = do
    graph <- fmap (buildGraph . words) (readFile defaultDictionary)
    -- undefined
    putStrLn (map fst $ edges $ submap "a" $ graph)


-- map fst $ drop 1 $ edges $ Dawg.submap "h" (buildGraph dict)

-- Dawg.edges subgraph $ Dawg.submap curword 

-- filterEdges word = 

-- | Returns all edges of a (sub)graph where the symbol (`a`) of an edge is an
-- element of `[a]`. So, for example if `[a]` is `"abc"` (or: `['a','b','c']`),
-- the result will consist of the subgraphs for the nodes which match `'a'`,
-- `'b'` and `'c'`.
partialEdges :: (Eq a, Enum a)
             => [a]               -- ^ List of symbols which an edge should match on
             -> DAWG a b c        -- ^ The graph where edges should be filtered out
             -> [(a, DAWG a b c)]
partialEdges w = filter ((`elem` w) . fst) . edges

-- | Returns all full and partial anagrams for a `Word`
solver :: AnagramGraph -> Word -> [Word]
solver g input = recSolver input g g "" []

recSolver :: Word         -- ^ The input for which anagrams should be generated
          -> AnagramGraph -- ^ The full graph which contains the whole dictionary
          -> AnagramGraph -- ^ The partial graph for the recursion
          -> Word         -- ^ The partial word for the recursion
          -> [Word]       -- ^ The accumulator for the result
          -> [Word]
recSolver input fg sg part_word acc
    | isWord && exitCondition = part_word:acc
    | isWord                  = concatMap (part_solve $ part_word:acc) part_edges
    | exitCondition           = acc
    | otherwise               = concatMap (part_solve acc) part_edges
    where
    -- is the current partial word a real word from the dictionary?
    isWord                    = isJust $ Dawg.lookup part_word fg
    -- is end of graph (== no more edges) reached?
    isEndOfWord               = null part_edges
    -- is the current partial word longer than the input word?
    isTooLong                 = length part_word >= length input
    exitCondition             = isEndOfWord || isTooLong
    part_edges                = partialEdges input sg
    part_solve acc' (nc, sg') = recSolver input fg sg' (part_word ++ [nc]) acc'

{-
solve' '' {graph:"dehlorw"} "helloworld" -> [('h', subgraph)]
    word: ""

    solve' 'h' {graph:"delorw"} "elloworld" -> [('e', subgraph), ('o', subgraph)]
        word: "h"

        solve' 'e' {graph:"dlorw"} "lloworld" -> [('e', subgraph), ('o', subgraph)]
            word: "he"
        solve' 'o' {graph:"delrw"} "lloworld" -> [('e', subgraph), ('o', subgraph)]
            word: "ho"
-}

-- h -> he -> hel -> hell -> hello -> hello

-- h -> ho -> hol -> holl -> hollo -> hollow

dict = ["hell","hello","he","hollow", "zynismn"]
