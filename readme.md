## An efficient anagram solver

Solving anagrams for long strings or sentences is a hard problem. A naive
implementation, which generates all substrings from a given input for looking
them up in a hash table does not scale.

A more efficient solver can be implemented using recursive backtracking and
a [trie](https://en.wikipedia.org/wiki/Trie) or, for more space efficiency,
a [directed acyclic word graph (DAWG)](https://en.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton).
This [answer](https://stackoverflow.com/a/880611) on stackoverflow provides an
helpful description of the recursive backtracking algorithm.

Since recursion is natural for Haskell, the implementation is fairly straight
forward. One caveat, however, is that two accumulators are necessary. One for
the current word which is partially built up from stepping through the nodes of
the graph, and another one for all resulting valid words.

There are plenty of comments in the source and I've tried to write the code as
concise and readable as possible. If there are any questions, do not hesitate to
ask me.

Some simple tests are also available as compile time option. Load the module
with the `ANAGRAM_SOLVER_TEST_CASES` macro set and call `runTests`.

```
$ cabal repl
# [..]
*Anagram.Solver> :set -DANAGRAM_SOLVER_TEST_CASES
*Anagram.Solver> :load *Anagram.Solver
# [..]
*Anagram.Solver> :t runTests
runTests :: IO ()
```
