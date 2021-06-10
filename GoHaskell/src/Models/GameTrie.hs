module Models.GameTrie where

type ID    = Int
type Level = Int

data TrieNode a = EmptyTrieNode
                | TrieNode {
                    value    :: a
                  , parent   :: ID
                  , children :: [ID]
                  , branch   :: ID
                  , level    :: Level
                  } deriving (Show, Eq)

-- If I'm gonna do this based on IDs then a hash map would be faster
type GameTrie a = [TrieNode a]

-- Collapses the path from a node, backtracking to the root.
collapsePath :: TrieNode a -> GameTrie a -> [a]
collapsePath _ [] = []
collapsePath _ gameTrie = [value (head gameTrie)]
