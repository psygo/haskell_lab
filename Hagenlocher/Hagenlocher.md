# Philipp Hagenlocher's Course on YouTube

[Haskell for Imperative Programmers - YouTube](https://www.youtube.com/playlist?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV)

---

**Table of Contents**

* [4. List Exercises](#4.-list-exercises)
  * [4. Directed Graphs](#4.-directed-graphs)
* [7. Partial Function Application (Currying)](#7.-partial-function-application-(currying))
* [8. Composisition](#8.-composisition)
* [9. Folding](#9.-folding)
* [12. Records](#12.-records)
* [13. Typeclasses](#13.-typeclasses)
* [17. Monads](#17.-monads)
  * [The Monad Laws](#the-monad-laws)
* [18. QuickCheck](#18.-quickcheck)
* [Infinite Lists](#infinite-lists)
* [20. Advanced Exercises](#20.-advanced-exercises)
  * [20.1. Infinite Tree with Tuples on Each Node](#20.1.-infinite-tree-with-tuples-on-each-node)
  * [20.2. Tree Traversal](#20.2.-tree-traversal)
  * [20.3. Word Search on a File](#20.3.-word-search-on-a-file)
* [21. `data`, `type` & `newtype`](#21.-`data`,-`type`-&-`newtype`)
* [22. `Either`](#22.-`either`)
* [25. Compiling Binaries](#25.-compiling-binaries)
* [26. Strictness, Thunks & `seq`](#26.-strictness,-thunks-&-`seq`)
* [27. Exceptions](#27.-exceptions)
* [28. Concurrency & Threads](#28.-concurrency-&-threads)
* [29. Semaphores](#29.-semaphores)
* [30. Software Transactional Memory (STM)](#30.-software-transactional-memory-(stm))
* [31. Weak Head Normal Form](#31.-weak-head-normal-form)
* [32. `DeepSeq`](#32.-`deepseq`)
* [33. Parallelism](#33.-parallelism)
* [34. Profiling](#34.-profiling)
* [35. Semigroup & Monoid](#35.-semigroup-&-monoid)
* [36. Category Theory (Functors, Applicatives, Monads)](#36.-category-theory-(functors,-applicatives,-monads))
  * [Laws](#laws)
  * [References](#references)
* [37. Arrows](#37.-arrows)
* [38. Monad Transformers](#38.-monad-transformers)
* [39. Formal Proofs](#39.-formal-proofs)
* [40. Termination Proofs](#40.-termination-proofs)
* [41. Formal Verification (using Isabelle)](#41.-formal-verification-(using-isabelle))
  * [Isabelle](#isabelle)
* [42. QuickSpec](#42.-quickspec)

---

## 4. List Exercises

### 4. Directed Graphs

Create a function `hasPath` that determines if a path from one node to another exists within a *directed* graph.

```hs
hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
  | x == y    = True
  | otherwise =
    let xs' = [ (n,m) | (n,m) <- xs, n /= x ] in
        or [ hasPath xs' m y | (n,m) <- xs, n == x ]
```

## 7. Partial Function Application (Currying)

All of the functions below are equivalent to one another:

```hs
add :: Int -> Int -> Int

add x y = x+y

add x = (\y -> x+y)

add = (\x -> (\y -> x+y))

add = \x y -> x+y
```

## 8. Composisition

```hs
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map
```

## 9. Folding

Focus on using tail-recursive functions (`foldl`) in order to avoid stackoverflow. However, stuff like `foldr` can be useful to create infinite data types.

> It's `fold` *to the left or right*, not from the left or right.

```hs
length = foldr (const $ (+) 1) 0
map    = foldr ((:) . f) []
```

`const` ignores part of an operation.

The main difference between `foldr` and `foldl` is how the accumulator behaves in the function.

```hs
foldr (\elem acc -> <term>) <start_acc> <list>
foldl (\acc elem -> <term>) <start_acc> <list>
```

[Fold - HaskellWiki](https://wiki.haskell.org/Fold)

An example of a Trie (prefix tree):

```hs
data Trie a = Leaf a | Node a [Trie a]
```

We can have 0..N subtrees. Folding the trie &mdash; preorder traversal &mdash;:

```hs
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t
```

## 12. Records

```hs
data Person = Person {
  name :: String,
  age  :: Int
}
-- The name and age function/getters are automatically generated.
-- And they are specific to each constructor

-- Instead of this more illegible version:

data Person = Person String Int
```

## 13. Typeclasses

> Use `:info` on GHCi to get information on each typeclass.

The `MINIMAL` directive describes the minimal set of functions you need to implement in order to make the typeclass work.

By default, `Eq` compares by structure, so comparing Celsius to Fahrenheit will not work if they are part of the same `data` type.

## 17. Monads

```hs
m >>= (\x -> ...)

-- Is equivalent to:

do
  x <- m
  ...

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my = mx >>= (\x -> my >>= (\y -> return $ x+y))

monadd mx my = do
  x <- mx
  y <- my
  return $ x+y

-- The implementation of the Maybe Monad:

instance Monad Maybe where
  m >>= f = case m of
                 Nothing -> Nothing
                 Just x -> f x
  return v = Just v

--

act >> ...

-- Is equivalent to:

do
  act
  ...
```

### The Monad Laws

1. Left Identity: `return a >>= k = k a`
1. Right Identity: `m >>= return = m`
1. Associativity: `m >>= (\x -> k x >>= h) = (m >>= k) >>= h`

## 18. QuickCheck

Install it with `cabal install QuickCheck`.

```hs
import Test.QuickCheck

prop a b = (a+b) == (b+a) -- run `quickCheck prop` on GHCi

prop xs = not (null xs) ==> (length $ tail xs) == ((length xs) - 1)
-- `quickCheck (verbose prop)` for details.

-- Use `===` to get structural equality not only result equality. (better debugging)
prop xs = not (null xs) ==> (length $ tail xs) === ((length xs) - 1)

-- This will give back statistics on what you wanted to collect.
propRev xs = collect (length xs) $ reverse xs === rev xs

-- Classify a specific use case of testing data
propRev xs = classify (length xs == 0) "empty" $ reverse xs === rev xs

-- Bump up the amount of tests
quickCheck (withMaxSuccess 10000 propRev)

-- Specifying better types so empty tuples don't distort the data for the tests
prop k v m = lookup k ((k,v):m) === Just v
  where types = (k :: Int, v :: Int)
```

QuickCheck is a *checker*, not a *verifier*. It cannot prove correctness.

## Infinite Lists

If you use Fibonacci like this:

```hs
f 0 = 0
f 1 = 1
f n = (f $ n-1) + (f $ n-2)

fibs = map f nat
```

We are going to get O(N^2) because we are not reusing previous results.

This is a much nicer definition:

```hs
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

This all could be used to create a lazy, infinite web crawler.

## 20. Advanced Exercises

### 20.1. Infinite Tree with Tuples on Each Node

The tuples correspond to how many left or right turns on the tree structure.

```hs
data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Building the infinite tree:
inv_tup_tree = aux (0,0)
  where
    aux (l,r) = Node (aux $ (l+1,r)) (l,r) (aux $ (l,r+1))

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut n Leaf = Leaf
cut n (Node l v r) = Node (cut (n-1) l) v (cut (n-1) r)
```

### 20.2. Tree Traversal

Write the functions `insert` and `inorder` for the binary tree type. Create a QuickCheck property that checks if the inorder traversal of a tree built with the insert function is sorted.

> Don't forget to use `import Data.List hiding (insert)`

```hs
data Tree a = Leaf | Node (Tree a) a (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Leaf = Node Leaf v Leaf
insert v (Node l vt r)
  | v <= vt = Node (insert v l) vt r
  | v >  vt = Node l vt (insert v r)

inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l v r) = (inorder l) ++ [v] ++ (inorder r)

prop_IIS xs = sort xs === xs'
  where
    types = xs :: [Int]
    xs'   = inorder $ foldr insert Leaf xs
```

### 20.3. Word Search on a File

```hs
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- so the prompt is immediately output
  sws <- getSearchWords
  putStr "File to search: "
  path <- getLine
  text <- readFile path
  let found = findStrings sws text
  let nfound = [ w | w <- sws, not $ w `elem` found ]
  mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found") found
  mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found") nfound

-- `mapM_` goes through all the elements of the foldable and applies the Monad (IO action)

getSearchWords :: IO [String]
getSearchWords =  do
  putStrLn "Specify the words to search:"
  aux
  where
    aux = do
      putStr "> "
      line <- getLine
      if line == "" then
        return []
      else do
        xs <- aux
        return $ line:xs

lower :: String -> String
lower = map toLower

findStrings :: [String] -> String -> [String]
findStrings sws text = [ w | w <- sws, (lower w) `elem` txtwords ]
  where
    ftext    = filter (\x -> isLetter x || isSpace x) text
    txtwords = map lower $ words ftext
```

## 21. `data`, `type` & `newtype`

1. `data`: new datatypes and constructors
1. `type`: just an alias for a type
1. `newtype`: it's like `data`, but with only one constructor and field.
    - The new type will be isomorphic with its underlying type.
    - New types are checked at compile time, yet ignored at runtime (no work done when pattern matching)

```hs
newtype Name = Name String -- it is like a String, but will be treated as a new type
```

## 22. `Either`

How could we generalize and add polymorphism to `data SomeData = Left Int | Right String`?

```hs
data Either a b = Left a | Right b

type SomeData = Either Int String

[Left 1, Right "Hello"] :: [SomeData]
```

There are 2 interesting functions coming with the `Either` type:

```hs
either :: (a -> c) -> (b -> c) -> Either a b -> c
-- Function for right and function for right
f = either (\l -> "Number") (\r -> r)

partitionEithers :: [Either a b] -> ([a], [b])
-- Good for error handling, very similar to `Maybe`
```

## 25. Compiling Binaries

- Your program needs a `main` IO action (`IO ()`).
- `--make` is good practice for resolving dependencies.

[6. Using GHC — Glasgow Haskell Compiler 8.4.1 User's Guide](https://downloads.haskell.org/~ghc/8.4.1/docs/html/users_guide/usage.html)

## 26. Strictness, Thunks & `seq`

```hs
seq :: a -> b -> b
seq a b = b
```

`seq` is the only function that forces the evaluation of `a` before returning `b`. That's enforced in the compiler.

```hs
f $! x = x `seq` f x
```

Returning values in IO actions is kind of useless, so that's where `$!` comes in.

`seq` might throw off some things the compiler expects though.

## 27. Exceptions

```hs
data MyError = ErrorA | Error B deriving Show

instance Exception MyError...
```

**Exceptions may be thrown from purely functional code, but may only be caught within the IO monad.**

```hs
catch :: Exception e => IO a -> (e -> IO a) -> IO a
-- e.g.: `catch ioAction exceptionHandler`
```

```hs
import Control.Exception

data MyError = Error deriving Show

instance Exception MyError

failing :: IO ()
failing = do
  throw Error

main :: IO ()
main = do
  catch failing (\(e :: MyError) -> do -- needs `-XScopedTypeVariables`
    --let t = (e :: MyError)
    putStrLn "Something went wrong!"
  )
```

There are other useful functions, like `catches` and `try`.

Prefer using `Maybe` and `Either`, since the are purely functional. However, exceptions are the only way of dealing with IO exceptions, threads and system management.

## 28. Concurrency & Threads

Haskell 98 doesn't support it.

In Concurrent Haskell, threads are created internally as well, so, from the system's perspective, only a couple threads might be running, when, in fact, Haskell is running thousands in its internal runtime.

So how do you communicate between threads?

```hs
import Control.Concurrent

forkIO :: IO () -> IO ThreadId
-- forkIO act

f :: Int -> Int -> IO ()
f a b = do
  let x = a+b
  putStrLn $! show x

main :: IO ()
main = do
  ...
  mVar <- newEmptyMVar
  forkIO $ f 1 2
  result <- takeMVar mVar
  putStrLn $ show result
  ...
```

`MVar`s are mutable locations that can be shared. Actions on `MVar`s are atomic (ACID).

```hs
newEmptyMVar :: IO (MVar a)

newMVar :: a -> IO (MVar a)

takeMVar :: MVar a -> IO a

putMVar :: MVar a -> a -> IO ()
```

**`mVar`'s are still lazy.**

There's also a channel/queue (`chan`) abstraction. You can use channels to send signals between the threads.

> Use the `--threaded` flag on GHC to enable this feature.

Use `mutex` to lock resources.

## 29. Semaphores

This is basically a way of dealing with race conditions when updating, for example an `MVar`.

> Locking synchronization mechanisms: so it might end up in a *deadlock*. This is an open problem, you have to debug and use intuition.

## 30. Software Transactional Memory (STM)

Instead of locking on the acquisition of the data, we check if anything has changed when we are going to finally commit to the storage.

- Critical sections -> Atomic transactions
- If a conflict arises on commit -> Rerun

Ensures atomicity, consistency and isolation. **No deadlocks are possible!**

> This is usually enforced on the hardware level nowadays actually.

This is implemented in GHC through the `STM` (`Control.Monad.STM`) monad, which came up in a paper with Marlow and SPJ.

## 31. Weak Head Normal Form

In Haskell evaluation, we use *outermost reduction* and *sharing* (memoization).

An expression is in normal form if and only if it is fully evaluated. Fully evaluated -> Fully executed. (Beta Reductions in Lambda calculus)

Weak Head Normal Form: (only holds for data values) an expression fully evaluated up to at least the first data constructor. `seq` evaluates to WHNF, for example.

`f x = <expr>` becomes `f = \x -> <expr>`.

> Use `:sprint` to examine this in GHCi.

## 32. `DeepSeq`

[Control.DeepSeq](https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/Control-DeepSeq.html)

If things don't evaluate to normal form and get stuck at WHNF, then use `deepseq`.

## 33. Parallelism

Parallelism != Concurrency

`cabal install parallel` modularizes parallelism.

Take a look at sparks for dealing with parallel computing.

[Control.Parallel.Strategies](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html)

Be careful with garbage collection though, it might occupy more processing than sparks do.

## 34. Profiling

1. Execution Time
1. Memory Usage
1. Garbage Collection
1. Code Coverage
1. Multicore Utilization

Use the `-p` flag to create a `.prof` file with a report.

`.hp` files can be converted to `.ps` with `hp2ps`.

The `-fhpc` can be used for code coverage.

```sh
hpc report Sort
hpc markup Sort # generates HTML with code view
```

Install the `haskell-threadscope` for analyzing performance.

Try to not optimize too much, because the compiler might not then be able to use its own optimizations.

## 35. Semigroup & Monoid

- Algebra: Domain + Operations (and some properties for the operations)
  - Examples: addition arithmetic, nothing algebra, perfect hash algebra.
- Magma: a set S with a closed binary operation. The operation produces elements that are in S.
- Semigroup: an associative magma
    - For lists, `<>` = `++`.
    - (a . b) . c = a . (b . c)
    ```hs
    class Semigroup a where
      (<>) :: a -> a -> a
    ```
- Monoid: semigroup with an identity element
    - (a . b) . c = a . (b . c)
    - e . a = a = a . e
    ```hs
    class Semigroup a => Monoid a where
      mempty :: a
      mappend :: a -> a -> a
      mconcat :: [a] -> a
      {-# MINIMAL mempty #-}
    ```
    - Typically `mconcat = foldr (<>) mempty`
        - For lists: `mconcat xss = [x | xs <- xss, x <- xs]`
    - `mempty` = []
    - Not every type has one monoid, for example, `Int`s have different identitiy values for `+` and `*`.
        - Then you can use `newtype` to trick things.

These algebraic abstractions give promises to programmers.

`<>` can be used for distributed computation, the ordering doesn't matter.

Mathematical properties make mathematical proofs possible.

## 36. Category Theory (Functors, Applicatives, Monads)

> Objects and Arrows.

Arrows:

1. Identity morphisms
1. Composition (necessary for a valid category)

Category:

- obj(C) := class of objects
- hom(C) := class of morphisms
- C(a,b) := All morphisms from a to b
- Composition of morphisms
    - h . f . g = (h . f) . g = h . (f . g)
    - f . 1 = 1 . f = f
- Identity morphism for each object

Monoids are categories for example.

Functors are mapping categories. Applying `map` is applying a functor as well.

The Monoidal Category:

```none
Category C, Functor X

X : C x C -> C (tensor product)
I : Identity object in obj(C)

Coherence conditions:

alpha_{A,B,C} := (A X B) X C = A X (B X C)
lambda_A      := I X A = A
rho_A         := A X I = A
```

You can also create a morphism between 2 monoidal categories, with a monoidal functor. Applicatives are equivalent to (lax) monoidal functors.

`fmap putStrLn getLine` = `putStrLn <$> getLine`, which is also equivalent to the `do` notation.

Finally, a **Monad is a Category + Functor (which goes from the category to itself)**. Monads implement applicative functors that don't go out of context:

```hs
class Applicative m => Monad (M :: * -> *)
  where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
  ```

### Laws

Functors:

```hs
fmap id = id                    -- Identity
fmap (f . g) = fmap f . famp go -- Composition
```

Applicatives:

```hs
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
```

Monads:

```hs
return a >>= f = f a                      -- Left Identity
m >>= return = m                          -- Right Identity
(m >>= f) >>= g = m >>= (\x -> f x >>= g) -- Associativity
```

### References

Category theory is at the heart of programming.

[Programming with Categories - YouTube](https://www.youtube.com/playlist?list=PLhgq-BqyZ7i7MTGhUROZy3BOICnVixETS)
[hmemcpy/milewski-ctfp-pdf: Bartosz Milewski's 'Category Theory for Programmers' unofficial PDF and LaTeX source](https://github.com/hmemcpy/milewski-ctfp-pdf)

[Seven Sketches in Compositionality by Brendan Fong & David I. Spivak](https://arxiv.org/pdf/1803.05316.pdf)

[Applicative programming with effect by Conor McBride & Ross Paterson](http://www.staff.city.ac.uk/~ross/pap...)

## 37. Arrows

> `import Control.Arrow`

A simple way of creating composition.

References:

- Programming with Arrows by Hughes
- Generalizing Monads to Arrows by Hughes

## 38. Monad Transformers

Can we combine the functionality of an `IO` monad with, say, the `Maybe` monad?

```hs
type IOMaybe a = IO (Maybe a)
```

This is great for passing on state.

State transformers are not yet stable.

## 39. Formal Proofs

We have to assum:

- GHC is correct
- Our computers are correct

> Noetherian Induction (Structural Induction)

There's also computational induction.

## 40. Termination Proofs

Even though they are possible and useful, it's related to the halting problem, which is undecidable.

It's also useful to check out the Total Functional Programming idea if you're interested in this topic.

## 41. Formal Verification (using Isabelle)

Instead of going from Haskell to proofs, we do it the other way around: the proof checker generates code based on metalogic from the proof.

### Isabelle

A purely functional language.

- Higher-Order Logic (HOL)
- Automatic solvers
- Code Generation (Haskell, OCaml, Scala, SML)
- Archive of formal proofs
- Used in verification of seL4 (microkernel)
    - The first verified kernel in the world.
- Features *real* math symbols with correct semantics.

```thy
theory Intro
  imports Main
begin

fun length :: "'a list => nat"
  where
    "length []     = 0" |
    "length (x#xs) = 1 + length xs"

value "length [1,0,0,0,2,3::int]"

fun foldr :: "('a => 'b => 'b) => 'b => 'a list => 'b"
  where
    "foldr f x []     = z" |
    "foldr f z (x#xs) = fx (foldr f z xs)"

fun const :: "'a => 'b => 'a"
  where
    "const a b = a"

lemma "length xs = foldr (const (\x . x + 1)) 0 xs"
  apply(induction xs)
    apply(auto)
  done
```

## 42. QuickSpec

[quickspec: Equational laws for free!](https://hackage.haskell.org/package/quickspec)

> It uses QuickCheck in the background.

`+s` in GHCi gives back the time it took to run a command.
