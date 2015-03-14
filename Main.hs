import Control.Applicative
import Data.Maybe
import Data.DAWG.Static
import qualified Data.DAWG.Static as Dawg
import qualified Data.List as L

import Debug.Trace

data WordMark = WordMark
    deriving (Eq, Ord, Read, Show)

type Word = String
type AnagramGraph = DAWG Char () WordMark

buildGraph :: [String] -> AnagramGraph
buildGraph = Dawg.fromList . flip zip (repeat WordMark) -- map (\s -> (s, WordMark))

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

isWord :: Word -> AnagramGraph -> Bool
isWord w = isJust . Dawg.lookup w

isEndOfWord :: Word -> AnagramGraph -> Bool
isEndOfWord w = L.null . Dawg.edges . Dawg.submap w

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

-- filter ((`elem` "helloworld") . fst) $ edges $ Dawg.submap "" (buildGraph dict)
filterEdges :: (Eq a, Enum a) => [a] -> DAWG a b c -> [(a, DAWG a b c)]
filterEdges w = filter ((`elem` w) . fst) . edges

solve _ _ _ wordacc _ [] = wordacc
solve fullgraph inword subgraph wordacc lastword (c:cs)
    | isWord curword fullgraph =
        trace ("isWord: " ++ curword ++ "; fedges: " ++ show (map fst fedges)) $
        concatMap (\(l,g) -> solve fullgraph inword g (curword:wordacc) (lastword ++ [c,l]) cs) fedges
    | otherwise            =
        trace ("! isWord: " ++ curword ++ "; fedges: " ++ show (map fst fedges)) $
        concatMap (\(l,g) -> solve fullgraph inword g wordacc (lastword ++ [c,l]) cs) fedges


    where
    -- fedges = trace ("fedges (" ++ curword ++ "): ") $ filterEdges inword $ Dawg.submap curword fullgraph
    fedges = filterEdges inword $ Dawg.submap curword fullgraph
    -- subgraph = trace curword $ Dawg.submap curword fullgraph
    curword = lastword ++ [c]

solve'' :: AnagramGraph -> Word -> [Word]
solve'' g inword = solve' g g "" inword []

solve' fg sg mword inword acc
    -- passiert nur beim letzten
    -- | isEndOfWord mword fg && isWord mword fg = mword:acc
    | isWord mword fg =
        let fes = map fst $ filterEdges inword sg in
        let fes' = filterEdges inword sg in
        if L.null fes'
            then mword:acc
            else concatMap (\(nc,sg') -> solve' fg sg' (mword ++ [nc]) inword (mword:acc))
                    $ filterEdges inword sg
        -- in res
        -- in trace ("isWord: mword: " ++ mword ++ "; res:" ++ show res ++ "; edges: " ++ show fes) $ res
    -- | isEndOfWord mword fg = acc
    | otherwise =
        let fes = map fst $ filterEdges inword sg in
        let fes' = filterEdges inword sg in
        if L.null fes'
            then acc
            else concatMap (\(nc,sg') -> solve' fg sg' (mword ++ [nc]) inword acc)
                $ filterEdges inword sg
        -- in res
        -- in trace ("otherwise: mword: " ++ mword ++ "; res:" ++ show res ++ "; edges: " ++ show fes) $ res

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
