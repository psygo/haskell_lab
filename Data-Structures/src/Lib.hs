module Lib
    ( printer
    , module Data.Stack
    ) where

import Data.Stack

import Data.Foldable

printer :: IO ()
printer = do
    -- Stack Basic Operations
    

    -- Algebra
    print $ StackCons [3,4,5] <> StackCons [1,2,3]
    print $ StackCons [1,2,3] <> mempty

    -- Foldable
    print $ foldr (*) 1 (StackCons [1,2,3])
    -- print ((fold $ StackCons [1,2,3]) :: [Int])
    
    -- Traversable
    print $ sequenceA (StackCons [Just 1, Just 2, Just 3])
    print $ sequenceA (StackCons [Just 1, Just 2, Just 3, Nothing])

    -- Functors
    print $ fmap (*2) (StackCons [1,2,3])

    -- Applicative
    print $ pure (*2) <*> StackCons [1,2,3]
    print $ (*) <$> StackCons [1,2,3] <*> StackCons [1,2,3]
    
    -- Monad
    print $ fmap (\a -> StackCons [a]) [1,2,3] -- wrong
    print $ StackCons [1,2,3] >>= (\s -> StackCons $ [2*s])
