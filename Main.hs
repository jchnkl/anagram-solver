import Control.Applicative
import Data.Maybe
import Data.DAWG.Static
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
type Word = String
-- type LabelGraph
type WordSet = HashSet Word
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

findAnagrams :: Word -> AnagramTable -> [Word]
findAnagrams w = fromMaybe [] . H.lookup (toLabel w)

bruteForceSolver :: AnagramTable -> Word -> [Word]
bruteForceSolver t w = concatMap (flip findAnagrams t)
                                 (filter (not . null) . L.subsequences $ L.sort w)

main :: IO ()
main = do
    graph <- fmap (buildGraph . words) (readFile defaultDictionary)
    putStrLn (map fst $ edges $ submap "a" $ graph)

-- | Returns all edges of a (sub)graph where the symbol (`a`) of an edge is an
-- element of `[a]`. So, for example if `[a]` is `"abc"` (or: `['a','b','c']`),
-- the result will consist of the subgraphs for the nodes which match `'a'`,
-- `'b'` and `'c'`.
partialEdges :: (Eq a, Enum a)
             => [a]               -- ^ List of symbols which an edge should match on
             -> DAWG a b c        -- ^ The graph where edges should be filtered out
             -> [(a, DAWG a b c)]
partialEdges w = filter ((`elem` w) . fst) . edges

-- -- | Returns all full and partial anagrams for a `Word`
-- solver :: AnagramTable -> WordGraph -> Word -> [Word]
-- solver t g input = let tags = recSolver g g ("",input) []
--                    in concatMap (\k -> H.lookupDefault [] k t) tags

-- | Find all labels (permutations) for a word
findLabels :: LabelSet -> WordGraph -> Word -> [Label]
findLabels ls g input = recSolver ls g g ("",input) []

-- -- | Returns all full and partial anagrams for a `Word`
-- solver :: WordGraph -> Word -> [Word]
-- solver g input = recSolver input g g "" []

-- TODO: parallel on recursion?

isWord w = isJust . G.lookup w

isWord' = S.member

recSolver :: -- Word         -- ^ The input for which anagrams should be generated
             LabelSet
          -> WordGraph -- ^ The full graph which contains the whole dictionary
          -> WordGraph -- ^ The partial graph for the recursion
          -- -> Depth        -- ^ The current recursion depth
          -> (Word, Word) -- ^ fst: The partial word for the recursion
                          --   snd: The input word for the recursion
          -> [Word]       -- ^ The accumulator for the result
          -> [Word]

-- recSolver []     fg sg part_word acc
--     | isWord = part_word:acc
--     | otherwise = acc
--     where
--     isWord                    = isJust $ G.lookup part_word fg

recSolver ls fg sg (part,[]) acc
    | isWord' part ls = part:acc
    | otherwise      = acc
    
recSolver ls fg sg (part,rem@(r:rs)) acc

    | isWord' part ls && isEndOfWord = part:acc

    | isWord' part ls                = concatMap (part_solve $ part:acc) part_edges

    | isEndOfWord           = acc

    | otherwise               = concatMap (part_solve acc) part_edges

    where
    -- partialWord = drop depth input

    -- is the current partial word a real word from the dictionary?
    -- isWord                    = isJust . flip G.lookup fg

    -- is end of graph (== no more edges) reached?
    isEndOfWord               = null part_edges

    -- is the current partial word longer than the input word?
    -- isTooLong                 = length part_word >= length input
    -- exitCondition             = isEndOfWord || isTooLong

    -- exitCondition             = isEndOfWord || length input <= depth

    part_edges                = partialEdges [r] sg
    -- part_edges                = partialEdges (part ++ rem) sg

    part_solve acc' (_, sg')  = recSolver ls fg sg' (part ++ [r], rs) acc'
    -- part_edges                = partialEdges input sg
    -- part_solve acc' (nc, sg') = recSolver input fg sg' (part_word ++ [nc]) acc'

-- subSeqs :: LabelSet -> Char -> [Char] -> WordGraph -> Word -> [Word] -> [Word]

subSeqs lset nodec rem@[] graph sg part acc
    | S.member part lset = tracex "rem       isWord" $ S.insert part acc
    | otherwise          = tracex "rem    otherwise" $ acc

    where
    tracex x = trace $ x ++ ": " ++ "{nodec="++show nodec++"}"
                       ++ "{rem="++show rem++"}" 
                       ++ "{part="++show part++"}" 
                       ++ "{acc="++show acc++"}" 
                       ++ "{edges="++show edges'++"}" 

    edges' = map fst $ validEdges sg

    remainingNodes = L.delete nodec rem
    validEdges = filter ((`L.elem` remainingNodes) . fst) . G.edges

subSeqs lset nodec rem graph sg part acc
    | null (validEdges graph)
        && S.member part lset = tracex "rem null isWord " $ S.insert part acc
    | null (validEdges graph) = tracex "rem null notWord" $ acc

    | S.member part lset = tracex "rem       isWord" $ nextStep (S.insert part acc)
    | otherwise          = tracex "rem      notWord" $ nextStep acc

    -- | isWord part = concatMap (nextStep $ part:acc) (validEdges $ G.submap [nodec] graph)
    -- | otherwise   = concatMap (nextStep acc) (validEdges $ G.submap [nodec] graph)

    where
    -- tracex x = if (not $ null part) && head part == 'h'
    tracex x = if True
        then trace $ x ++ ": " ++ "{nodec="++show nodec++"}"
                       ++ "{rem="++show remainingNodes++"}" 
                       ++ "{part="++show part++"}" 
                       -- ++ "{acc="++show acc++"}" 
                       ++ "{edges="++show edges'++"}" 
        else id

    edges' = map fst $ validEdges sg

    remainingNodes = L.delete nodec rem
    validEdges = filter ((`L.elem` remainingNodes) . fst) . G.edges

    -- nextStep acc' = concatMap (nextSeqs acc') (validEdges $ G.submap [nodec] graph)
    nextStep acc' = S.unions $ map (nextSeqs acc') (validEdges sg)
    nextSeqs acc' (c,g) = subSeqs lset c remainingNodes g g (part ++ [c]) acc'


-- test a = solver <$> getTable <*> getGraph <*> pure a

-- test' w = do
--     table <- getTable
--     graph <- getGraph
--     let a = solver table graph w
--         b = bruteForceSolver table w
--     return $ a == b

subSeqTest = do
    labels <- getLabelSet
    graph <- getGraph
    let _ = seq () $ S.member "ehlo" labels
    let _ = seq () $ map fst $ G.edges  $ G.submap "ehllo" graph
    return $ subSeqs labels 'e' "ehllo" graph graph "" S.empty

dict = ["hell","hello","he","eh","eel","lee","hollow","el","le","low","zynismn"]

-- yeah nodec (c:cs)
--     |

-- map fst $ concatMap (filter (('o'==) . fst) . G.edges . snd)
--         $ filter (('h'==) . fst) . G.edges $ G.fromLang dict

-- *Main> map fst $ concatMap (filter (('e'==) . fst) . G.edges . snd)
--                $ filter (('h'==) . fst) . G.edges $ G.fromLang dict
-- "e"
-- *Main> map fst $ concatMap (filter (('o'==) . fst) . G.edges . snd)
--                $ filter (('h'==) . fst) . G.edges $ G.fromLang dict
-- "o"
-- *Main> map fst $ concatMap (filter (('l'==) . fst) . G.edges . snd)
--                $ filter (('h'==) . fst) . G.edges $ G.fromLang dict
-- ""

-- yeah input fg sg (c:cs) = case L.find ((c==) . fst) $ G.edges sg of
--     Nothing -> yeah input fg sg (dropUntil (c/=) input)
--     Nothing -> []
--     Just (c',g') -> yeah input fg g' cs
--     where
--     dropUntil f = drop 1 . dropWhile f

-- *Main> map fst . G.edges . G.submap "h" $ buildGraph dict
-- "eo"
-- *Main> map fst $ concatMap (\(c,g) -> G.edges $ G.submap [c] g) $ G.edges . G.submap "h" $ buildGraph dict
-- ""
-- *Main> map fst $ concatMap (\(c,g) -> G.edges $ G.submap "e" g) $ G.edges . G.submap "h" $ buildGraph dict
-- ""
-- *Main> map fst $ concatMap (\(c,g) -> G.edges $ G.submap "l" g) $ G.edges . G.submap "h" $ buildGraph dict
-- "ll"


-- yeah' (c:cs) g = yeah [] cs (c,g)

-- yeah acc part (c,g) = case L.find ((c==) . fst) (G.edges g) of
--     Nothing      -> acc
--     -- g' => edges for c
--     Just  (_,g') -> concatMap (\(c',g') ->
--         -- c' => node for edge of c
--         -- g' => subgraph for c'
--         if c' `L.elem` part
--             then yeah (c':acc) (c' `L.delete` part) (c',g')
--             else acc
--             ) (G.edges g') -- (filter ((c'==) . fst) $ G.edges g')

yeah :: WordSet -> String -> WordSet -> String -> WordGraph -> WordSet -- [String]
yeah lset wort acc [] _
    | isWord wort = S.insert wort acc
    | otherwise   = acc
    where
    isWord = flip S.member lset
yeah lset wort acc part g
        --                     trace ("yeah: part: " ++ part
        --                            ++ "; validEdges: " ++ show (map fst $ validEdges g)
        --                            ++ "; wort: " ++ wort
        --                           )
        --         $ concatMap freak . validEdges $ g
        | L.null validEdges && isWord wort = S.insert wort acc
        | L.null validEdges = acc
        | otherwise = S.unions $ map freak validEdges

    where
    isWord = flip S.member lset
    -- isValidEdge :: (Char, WordGraph) -> Bool
    isValidEdge (c',_) = c' `elem` part
    -- validEdges :: WordGraph -> [(Char, WordGraph)]
    validEdges = filter isValidEdge (G.edges g)

    freak :: (Char, WordGraph) -> WordSet -- [String]
    freak (c',g') -- = trace ("freak: " ++ [c'])
        | isWord wort = yeah lset (wort++[c']) (S.insert wort acc) (c' `L.delete` part) g'
        | otherwise   = yeah lset (wort++[c']) acc (c' `L.delete` part) g'

                  -- $ yeah (wort++[c']) acc (c' `L.delete` part) g'

    -- Just (c',g') -> if c' `L.elem` part
    --                     then concatMap (yeah (c':acc) (c' `L.delete` part)) (G.edges g')
    --                     else c':acc

-- yeah [] "elloworld" ('h',g) = case .. of
--     Nothing -> []
--     Just ('h',g') -> 

-- l -> [e,o]

-- rec word (c:cs) g = concatMap (rec' [] $ c `L.delete` word) (G.edges $ G.submap [c] g)
--
-- rec' acc part (c',g') = map fst $ concatMap (\c'' -> G.edges $ G.submap [c''] g') part
--
-- filt part (c',g') = filter ((`L.elem` part) . fst) $ G.edges g'

-- rec input (c,g) = (\c' -> G.submap [c'] g)  (c `L.delete` input)


-- "helloworld"
-- "elloworld" -> 'h'

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

{-
e -> h -> l -> o

.---------.
|         v
e -> h -> l -> o
^----|    |----^


-- h -> he -> hel -> hell -> hello -> hello

-- h -> ho -> hol -> holl -> hollo -> hollow

-}
