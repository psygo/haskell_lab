module Test.GameTrieTests where

import Test.Utils.TestInfra

import Models.GameTrie

runGameTrieTests :: IO ()
runGameTrieTests =  runTests tests

tests :: Tests
tests =  Tests
  [
    gameTrieTest0
  , gameTrieTest1
  ]

gameTrieTest0 :: Test
gameTrieTest0 =  Test
  "Collapsing an empty game trie yields an empty list"
  (null
    (
      collapsePath
        (
          TrieNode
            {
              value    = "A1"
            , parent   = -1
            , children = []
            , branch   = 1
            , level    = 1
            }
        )
        []
    )
  )

gameTrieTest1 :: Test
gameTrieTest1 =  Test
  "Collapsing a game trie with only one node yields the value of the node"
  (
    collapsePath
      (
        TrieNode
          {
            value    = "A1"
          , parent   = EmptyTrieNode
          , children = []
          , branch   = 1
          , level    = 1
          }
      )
      [
        TrieNode
          {
            value    = "A1"
          , parent   = EmptyTrieNode
          , children = []
          , branch   = 1
          , level    = 1
          }
      ]
    ==
    ["A1"]
  )

gameTrieTest2 :: Test
gameTrieTest2 =  Test
  "Collapsing a game trie with 2 nodes in a single branch"
  (
    collapsePath
      (
        TrieNode
          {
            value  = "B1"
          , parent = EmptyTrieNode
          , children = []
          , branch = 1
          , level  = 2
          }
      )
      [
        TrieNode
          {
            value    = "A1"
          , parent   = EmptyTrieNode
          , children = ["B1"]
          , branch   = 1
          , level    = 1
          },
        TrieNode
          {
            value    = "B1"
          , parent   = EmptyTrieNode
          , children = []
          , branch   = 1
          , level    = 2
          }
      ]
    ==
    ["A1","B1"]
  )
