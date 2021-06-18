{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
--------------------------------------------------------------------------------
module Data.Stack where

import Data.Foldable
--------------------------------------------------------------------------------
-- TODO: Add stackoverflow
newtype Stack a = StackCons [a]
  deriving ( Show
           , Foldable
           , Traversable)

class StackClass s where
  push    :: a -> s a -> s a
  pop     :: s a -> s a
  top     :: s a -> Maybe a
  isEmpty :: s a -> Bool

instance StackClass Stack where
  push item (StackCons items) = StackCons (item:items)

  pop (StackCons [])        = StackCons []
  pop (StackCons (_:items)) = StackCons items

  top (StackCons []) = Nothing
  top (StackCons (item:_)) = Just item

  isEmpty (StackCons items) = null items
--------------------------------------------------------------------------------
-- Category Theory

instance Functor Stack where
  -- fmap :: (a -> b) -> Stack [a] -> Stack [b]
  fmap _ (StackCons [])     = StackCons []
  fmap g (StackCons (x:xs)) = StackCons (g x : fmap g xs)

instance Applicative Stack where
  -- pure :: a -> Stack [a]
  pure x = StackCons [x]

  -- (<*>) :: Stack (a -> b) -> Stack a -> Stack b
  (StackCons fs) <*> (StackCons ss) = StackCons [ f s | f <- fs, s <- ss ]

instance Monad Stack where
  -- (>>=) :: Stack a -> (a -> Stack b) -> Stack b
  (StackCons [])     >>= _ = StackCons []
  (StackCons (s:ss)) >>= f = f s <> (StackCons ss >>= f)
--------------------------------------------------------------------------------
-- Algebras

instance Semigroup (Stack a) where
  -- (<>) :: Stack a -> Stack a -> Stack a
  (StackCons ls) <> (StackCons rs) = StackCons (ls ++ rs)

instance Monoid (Stack a) where
  -- mempty :: Stack a
  mempty = StackCons []

-- instance Foldable Stack where
  -- foldr :: Monoid (Stack a) => (a -> Stack a) -> Stack a -> Stack a
  -- foldr _ last (StackCons [])     = last
  -- foldr f last (StackCons (s:ss)) = s `f` foldr f last ss

  -- fold :: Monoid [a] => Stack [a] -> [a]
  -- fold (StackCons []) = []
  -- fold (StackCons (s:ss)) = s : fold (StackCons ss) -- wrong

-- instance Traversable Stack where
  -- traverse :: Applicative f => (Stack a -> f (Stack b)) -> Stack a -> f (Stack b)
  -- traverse _ (StackCons []) = pure $ StackCons []
  -- traverse f (StackCons ss) = StackCons <$> f x 

  -- sequenceA :: Applicative f => Stack (f a) -> f (Stack a)
  -- sequenceA [] = f <$> []
--------------------------------------------------------------------------------
