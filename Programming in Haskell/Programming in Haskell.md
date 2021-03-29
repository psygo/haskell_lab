# Programming in Haskell

---

**Table of Contents**

* [1. Introduction](#1.-introduction)
  * [Historical Background](#historical-background)
* [2. First Steps](#2.-first-steps)
* [3. Types and Classes](#3.-types-and-classes)
* [4. Defining Functions](#4.-defining-functions)
  * [Conditional Expressions](#conditional-expressions)
  * [Lambda Expressions](#lambda-expressions)
  * [Operator Sections](#operator-sections)
* [5. List Comprehensions](#5.-list-comprehensions)
  * [The `zip` function](#the-`zip`-function)
* [6. Recursive Functions](#6.-recursive-functions)
  * [Advice on creating recursive functions](#advice-on-creating-recursive-functions)
* [7. Higher-order functions](#7.-higher-order-functions)
  * [The `foldr` function](#the-`foldr`-function)
  * [The `foldl` function](#the-`foldl`-function)
  * [The Composition Operator](#the-composition-operator)
* [8. Declaring Types and Classes](#8.-declaring-types-and-classes)
  * [Types](#types)
  * [Data Declarations](#data-declarations)
  * [`newtype` declarations](#`newtype`-declarations)
  * [Recursive Types](#recursive-types)
  * [Class and Instance Declarations](#class-and-instance-declarations)
    * [Derived Classes](#derived-classes)
* [9. The Countdown Problem](#9.-the-countdown-problem)
* [10. Interactive Programming](#10.-interactive-programming)
  * [Basic Actions](#basic-actions)
  * [Sequencing](#sequencing)
* [12. Monads and More](#12.-monads-and-more)
  * [Functors](#functors)
    * [Examples](#examples)
  * [Functor Laws](#functor-laws)
  * [Applicatives](#applicatives)
    * [Applicative Laws](#applicative-laws)
  * [Monads](#monads)
    * [Examples](#examples)
    * [Monad Derivation](#monad-derivation)
    * [Generic Functions](#generic-functions)
    * [Monad Laws](#monad-laws)
* [13. Monadic Parsing](#13.-monadic-parsing)
* [Making Choices](#making-choices)
* [14. Foldables and Friends](#14.-foldables-and-friends)
  * [Foldables](#foldables)
  * [Generic Functions](#generic-functions)
  * [Traversables](#traversables)
* [15. Lazy Evaluation](#15.-lazy-evaluation)
  * [Evaluation Strategies](#evaluation-strategies)
  * [Lambda Expressions](#lambda-expressions)

## 1. Introduction

What is functional programming? Opinions differ, and it is difficult to give aprecise definition. Generally speaking, however, functional programming can beviewed as a style of programming in which the basic method of computation isthe application of functions to arguments. In turn, a functional programminglanguage is one that supports and encourages the functional style.

In general, programming languages such as Java in which the basic method ofcomputation is changing stored values are called imperative languages, becauseprograms in such languages are constructed from imperative instructions thatspecify precisely how the computation should proceed.

However, many imperative languages do not encourage programming in the functional style.

Haskell programs are often between two and ten times shorter than programs written in other languages.

> More generally, higher-order functions can be used to define domain-specific languages within Haskell itself, such as for list processing, interactive programming, and parsing.

> Lazy evaluation ensures that programs terminate whenever possible.

> For example, the language provides a range of library functions that can be used with any type that is functorial , applicative, monadic, foldable, or traversable.

> Because programs in Haskell are pure functions, simple equational reason-ing techniques can be used to execute programs, to transform programs, toprove properties of programs, and even to calculate programs directly fromspecifications of their intended behaviour. Equational reasoning is partic-ularly powerful when combined with the use of induction to reason aboutfunctions that are defined using recursion.

### Historical Background

**Do check the full timeline, it's very instructive.**

- Lisp had some influences from the lambda calculus, but still retained the concept of variable assignment as a central feature of the language.
- In the 1990s, Philip Wadler and others developed the concept of type classes to support overloading, and the use of monads to handle effects, two of the main innovative features of Haskell.

## 2. First Steps

- Function application has higher priority than all other operators in the language.
- For example, if you wish to use vim you would enter :set editor vim. The command :type is explained in more detail in the next chapter.
- If desired, such grouping can be made explicit by enclosing a sequence of definitions in curly parentheses and separating each definition by a semi-colon.
- Haskell assumes that tab stops are 8 characters wide.
- Nested comments begin and end with the symbols ``{-`` and ``-}``.

Haskell Keywords:

- case
- class
- data
- default
- deriving
- do
- else
- foreign
- if
- import
- in
- infix
- infixl
- infixr
- instance
- let
- module
- newtype
- of
- then
- type
- where

## 3. Types and Classes

- We use the notation `v :: T` to mean that `v` is a value in the type `T`, and say that `v` has type `T`.
- More generally, the symbol `::` can also be used with expressions that have not yet been evaluated, in which case the notation `e :: T` means that evaluation of the expression `e` will produce a value of type `T`.
- See page 23 for the summary of how type inference works.
- The downside of type safety is that some expressions that evaluate successfully will be rejected on type grounds.
- We conclude this section by noting that a single number may have more than one numeric type. For example, the number 3 could have type `Int`, `Integer`, `Float` or `Double`.
- The precision on `floats` and `doubles` depends upon the its size on memory.
- The number of elements in a list is called its length. The list [] of length zero is called the empty list, while lists of length one, such as [False], [’a’], and [[]] are called singleton lists.
- A tuple is a finite sequence of components of possibly different types, with the components being enclosed in round parentheses and separated by commas.
    - The number of components in a tuple is called its *arity*.
    - Tuples of arity one, such as `(False)`, are not permitted because they would conflict with the use of parentheses.
- Note that there is no restriction that functions must be total on their argument type, in the sense that there may be some arguments for which the result is not defined.
- Currying
    - Functions with multiple arguments can also be handled in another, perhaps less obvious way, by exploiting the fact that functions are free to return functions as results.
    - Curried functions can be used to create partially applied ones, something you cannot do if you use tuples to hold multiple arguments.
    - `Int -> Int -> Int -> Int` means `Int -> (Int -> (Int -> Int))`
- The idea that length can be applied to lists whose elements have any type is made precise in its type by the inclusion of a type variable. Type variables must begin with a lower-case letter.
- Overloaded Types
    - Class constraints: `(+) :: Num a => a -> a -> a`
    - Numbers themselves are also overloaded. For example, 3 :: Num a => a meansthat for any numeric type a, the value 3 has type a. In this manner, the value 3could be an integer, a floating-point number, or more generally a value of any numeric type, depending on the context in which it is used.
- Recall that a type is a collection of related values. Building upon this notion, a class is a collection of types that support certain overloaded operations called methods.
- Note that the Num class does not provide a division method, but as we shall now see, division is handled separately using two special classes, one for integral numbers and one for fractional numbers.
    - For efficiency reasons, a number of prelude functions that involve both lists and integers (such as take and drop) are restricted to the type Int of finite- precision integers, rather than being applicable to any instance of the Integral class. If required, however, such generic versions of these functions are provided as part of an additional library file called Data.List.


> A class (`==`) is a collection of types (`Bool`, `Num`, etc.). A class contains types.

## 4. Defining Functions

### Conditional Expressions

There are many ways of creating conditional expressions:

- `abs n = if n >= 0 then n else -n`
    - You can nest `if`s to get the effect of if-else's.
    - Note that unlike in some programming languages, conditional expressions in Haskell must always have an else branch, which avoids the well-known dangling else problem. 
- Another way of doing it: *guarded equations*:
    ```hs
    abs n | n >= 0    = n
          | otherwise = -n
    ```
    - The symbol `|` is read as *such that*.
    - The guard otherwise is defined in thestandard prelude simply by `otherwise = True`.
- Another way of dealing with conditional expressions is through *pattern matching*.
    - This version also has the benefit that, under lazy evaluation as discussed in chapter 15, if the first argument is `False`, then the result `False` is returned without the need to evaluate the second argument.
- *Tuple Patterns*
- *List Patterns*
    - Up to this point, we have viewed lists as a primitive notion in Haskell. In fact they are not primitive as such, but are constructed one element at a time starting from the empty list [] using an operator : called cons that cons tructs a new list by prepending a new element to the start of an existing list. (`1:2:[] == [1,2]`)
    - Note that cons patterns must be parenthesised, because function applicationhas higher priority than all other operators in the language.

> Note that Haskell does not permit the same name to be used for more than one argument in a single equation. **Use guards to get around it.
```hs
b && c | b == c    = b
       | otherwise = False
```
### Lambda Expressions

As well as being interesting in their own right, lambda expressions have a number of practical applications. First of all, they can be used to formalise the meaning of curried function definitions.

```hs
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)
```

Moreover, rewriting the original definition in this manner also has the benefit
that the type for the function and the manner in which it is defined now have
the same syntactic form, namely `? -> (? -> ?)`.

Secondly, lambda expressions are also useful when defining functions that re-
turn functions as results by their very nature, rather than as a consequence of
currying. For example, the library function const that returns a constant func-
tion that always produces a given value can be defined as follows:

```hs
const :: a -> (b -> a)
const x = \_ -> x
```

### Operator Sections

In general, if `#` is an operator, then expressions of the form `(#)`, `(x #)`, and `(# y)` for arguments x and y are called sections.

```hs
(#) = \x -> (\y -> x # y)
(x #) = \y -> x # y
(# y) = \x -> x # y
```

- Operator sections are great for compactness.
- Sections are necessary when stating the type of operators, because an operator itself is not a valid expression in Haskell.

## 5. List Comprehensions

In mathematics, the *comprehension* notation can be used to construct new sets from existing sets.

The symbol `|` is read as *such that*, `<-` is read as is *drawn from*, and the expression `x <- [1..5]` is called a *generator*.

List comprehensions can also use logical expressions called *guards* to filter the values produced by earlier generators.

### The `zip` function

The library function `zip` produces a new list by pairing successive elements from two existing lists until either or both lists are exhausted.

```hs
> zip [’a’,’b’,’c’] [1,2,3,4]
[(’a’,1),(’b’,2),(’c’,3)]
```

> The term comprehension comes from the axiom of comprehension in set theory.

## 6. Recursive Functions

Defining functions using recursion also allows properties of those functions to be proved using the simple but powerful technique of induction.

> Defining recursive functions is like riding a bicycle: it looks easy when someone else is doing it.

### Advice on creating recursive functions

1. Define the type
1. Enumerate the cases
1. Define the simple cases
1. Define the other cases
1. Generalise and simplify

## 7. Higher-order functions

Formally speaking, a function that takes a function as an argument or returnsa function as a result is called a higher-order function. In practice, however,because the term curried already exists for returning functions as results, theterm higher-order is often just used for taking functions as arguments. It is thislatter interpretation that is the subject of this chapter.Formally speaking, a function that takes a function as an argument or returnsa function as a result is called a higher-order function. In practice, however,because the term curried already exists for returning functions as results, theterm higher-order is often just used for taking functions as arguments. It is this latter interpretation that is the subject of this chapter.

Using higher-order functions considerably increases the power of Haskell, byallowing common programming patterns to be encapsulated as functions withinthe language itself. More generally, higher-order functions can be used to de- fine domain-specific languages within Haskell.

### The `foldr` function

The typical recursive pattern:

```hs
f []     = v
f (x:xs) = x # f xs
```

Where `#` denotes an arbitrary operator. You could use `foldr` to simplify this pattern even more:

```hs
sum :: Num a => [a] -> a
sum = foldr (+) 0
```

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```

In practice, however, it is best to think of the behaviour of foldr f v in a non-recursive manner, as simply replacing each cons operator in a list by the function f, and the empty list at the end by the value v. For example, applying the function foldr (+) 0 to the list 1 : (2 : (3 : [])) gives the result 1 + (2 + (3 + 0)) in which : and [] have been replaced by + and 0, respectively.

```hs
length :: [a] -> Int
length = foldr (\_ n -> 1+n) 0
```

### The `foldl` function

The other recursive pattern:

```hs
f v []     = v
f v (x:xs) = f (v # x) xs
```

When a function can be defined using both foldr and foldl, as in the aboveexamples, the choice of which definition is preferable is usually made on groundsof efficiency and requires careful consideration of the evaluation mechanism underlying Haskell, which is discussed in chapter 15.

```hs
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs
```

### The Composition Operator

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

```hs
twice f x = f (f x)
twice f = f . f
```

> Composition is associative.

```hs
id :: a -> a
id = \x -> x
```

That is, id is the function that simply returns its argument unchanged, and hasthe property that id . f “ f and f . id “ f for any function f. The identityfunction is often useful when reasoning about programs, and also provides a suitable starting point for a sequence of compositions. For example, the composition of a list of functions can be defined as follows:

```hs
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
```

> We conclude by noting that the case mechanism of Haskell that is used in the above definition allows pattern matching to be used in the body of a definition, and is sometimes useful for avoiding the need to introduce an extra function definition just for the purposes of performing pattern matching.

## 8. Declaring Types and Classes

### Types

The simplest way of declaring a new type is to introduce a new name for an existing type, using the `type` mechanism of Haskell.

- Type declarations can be *nested*, but **not** *recursive*.
    -  If required, recursive types can be declared using the more powerful `data` mechanism, which will be introduced in the next section.

### Data Declarations

A completely new type, as opposed to a synonym for an existing type, can be declared by specifying its values using the data mechanism of Haskell.

```hs
data Bool = False | True
```

In such declarations, the symbol `|` is read as *or*, and the new values of the type are called *constructors*. As with new types themselves, the names of newconstructors must begin with a capital letter. Moreover, the same constructorname cannot be used in more than one type.

```hs
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
```

Because of their use of arguments, the constructors Circle and Rect are actu- ally constructor functions , which produce results of type Shape from arguments of type Float.

> The difference between normal functions and constructor functions is that the latter have no defining equations, and exist purely for the purposes of building pieces of data.

Not surprisingly, data declarations themselves can also be parameterised. For example, the standard prelude declares the following type:

```hs
data Maybe a = Nothing | Just a
```

### `newtype` declarations

If a new type has a single constructor with a single argument, then it can also be declared using the `newtype` mechanism.

```hs
newtype Nat = N Int
```

- Using newtype rather than type means that `Nat` and `Int` are differenttypes rather than synonyms, and hence the type system of Haskell ensures thatthey cannot accidentally be mixed up in our programs, for example by using an integer when we expect a natural number.
- Using newtype rather than data brings an efficiency benefit, because newtype constructors such as N do not incur any cost when programs are evaluated, as they are automatically removed by the compiler once type checking is completed. In summary, using newtype helps improve type safety, without affecting performance.

### Recursive Types

```hs
data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- This might yield a legitimate sorted list if the tree is a search tree.
flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                 = x == y
occurs' x (Node l y r) | x == y    = True
                       | x <  y    = occurs x l
                       | otherwise = occurs x r
```

Note that under lazy evaluation, if either of the first two conditions in the node case is True, then the result True is returned without the need to evaluate the remaining conditions.

### Class and Instance Declarations

```hs
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False

-- Extending an existing class:
class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max             :: a -> a -> a

  min x y | x <= y    = x
          | otherwise = y

  max x y | x <= y    = y
          | otherwise = x
```
#### Derived Classes

> Note that for the purposes of deriving instances of the class Ord of ordered types, the ordering on the constructors of a type is determined by their position in its declaration.

> In the case of constructors with arguments, the types of these arguments must also be instances of any derived classes.

>  In the same manner as lists and tuples, values built using constructors with arguments are ordered lexicographically.

## 9. The Countdown Problem

> Given a sequence of numbers and a target number, attempt to con- struct an expression whose value is the target, by combining one or more numbers from the sequence using addition, subtraction, multiplication, division and parentheses.

## 10. Interactive Programming

In the early days of computing, most programs were batch programs that were run in isolation from their users, to maximise the amount of time the computer was performing useful work.

Hence, given a suitable type World whose values represent states of the world, the notion of an interactive program can be represented by a function of type World -> World, which we abbreviate as IO (short for input/output) using the following type declaration:

```hs
type IO = World -> World

-- Generalizing:
type IO a = World -> (a,World)
```

Expressions of type `IO a` are called actions. 

In addition to returning a result value, interactive programs may also requireargument values. However, there is no need to generalise the IO type furtherto take account of this, because this behaviour can already be achieved by ex-ploiting currying. For example, an interactive program that takes a characterand returns an integer would have type Char -> IO Int, which abbreviates the curried function type Char -> World -> (Int,World).

### Basic Actions

The function return provides a bridge from pure expressions without side effects to impure actions with side effects.

```hs
return :: a -> IO a
return v = ...
```

### Sequencing

```hs
do v1 <- a1
   v2 <- a2
   .
   .
   .
   vn <- an
   return (f v1 v2 ... vn)
```

- As with list comprehensions, the expressions `vi <- ai` are called generators, because they generatevalues for the variables vi.
- If the result value produced by a generator `vi <- ai` is not required, the generator can be abbreviated simply by ai,which has the same meaning as writing `_ <- ai`.

For example, an action that reads three characters, discards the second, and returns the first and third as a pair can now be defined as follows:

```hs
act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)
```
```hs
getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else
                do xs <- getLine
                   return (x:xs)
```

## 12. Monads and More

- Functors
- Applicatives
- Monads

### Functors

More generally, the idea of mapping a function over each element of a datastructure isn’t specific to the type of lists, but can be abstracted further to a widerange of parameterised types. The class of types that support such a mappingfunction are called functors. In Haskell, this concept is captured by the following class declaration in the standard prelude:

```hs
class Functor f where
   fmap :: (a -> b) -> f a -> f b
```

That is, for a parameterised type f to be an instance of the class Functor, it must support a function fmap of the specified type. The intuition is that fmaptakes a function of type a -> b and a structure of type f a whose elements havetype a, and applies the function to each such element to give a structure of typef b whose elements now have type b. The fact that f must be a parameterisedtype, that is, a type that takes another type as a parameter, is determinedautomatically during type inference by virtue of the application of f to the types a and b in the specified type for fmap in the class declaration.

#### Examples

As we would expect, the type of lists can be made into a functor by simply
defining fmap to be the function map:

```hs
instance Functor [] where
   -- fmap :: (a -> b) -> [a] -> [b]
   fmap = map

instance Functor Maybe where
   -- fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap _ Nothing = Nothing
   fmap g (Just x) = Just (g x)
```

However, not all instances fit this pattern. For example,
the IO type is not a container type in the normal sense of the term because its
values represent input/output actions whose internal structure we do not have
access to, but it can readily be made into a functor:

```hs
instance Functor IO where
   -- fmap :: (a -> b) -> IO a -> IO b
   fmap g mx = do {x <- mx; return (g x)}
```

**The main benefits**:

- The function fmap can be used to process the elements of any structure that is functorial. That is, we can use the same name for functions that are essentially the same, rather than having to invent a separate name for each instance.
- We can define generic functions that can be used with any functor. For example, our earlier function that increments each integer in a list can be generalised to any functorial type by simply using fmap rather than map:

```hs
inc :: Functor f => f Int -> f Int
inc = fmap (+1)

> inc (Just 1)
Just 2

> inc [1,2,3,4,5]
[2,3,4,5,6]

> inc (Node (Leaf 1) (Leaf 2))
Node (Leaf 2) (Leaf 3)
```

### Functor Laws

The 2 laws:

```hs
fmap id      = id
fmap (g . h) = fmap g . fmap h
```

Note,however, that the two occurrences of id in this equation have different types: on the left-hand side id has type a -> a and hence fmap id has type f a -> f a,which means that the id on the right-hand side must also have type f a -> f a in order for the equation to be well-typed.

We will see how to formally prove such properties when we consider tech-niques for reasoning about programs in chapter 16. In fact, for any parameterisedtype in Haskell, there is at most one function fmap that satisfies the requiredlaws. That is, if it is possible to make a given parameterised type into a functor,there is only one way to achieve this. Hence, the instances that we defined for lists, Maybe, Tree and IO were all uniquely determined.

### Applicatives

Suppose now that we wish to generalise this idea to allow functions with any
number of arguments to be mapped, rather than being restricted to functions
with a single argument.

```hs
fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

In fact, using the idea of currying, it turns out that a version of fmap for functions with any desired number of arguments can be constructed in terms of two basic functions with the following types:

```hs
pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b

g <*> x <*> y <*> z

-- applicative style:
pure g <*> x1 <*> x2 <*> ... <*> xn
```

The class of functors that support pure and <*> functions are called applicative functors, or applicatives for short. In Haskell, this concept is captured by the following built-in class declaration:

```hs
class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
   -- pure :: a -> Maybe a
   pure = Just

   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Nothing <*> _ = Nothing
   (Just g) <*> mx = fmap g mx
```

**The one line of code that justifies all this gymnastics**:

```hs
pure (+) <*> Nothing <*> Just 2
Nothing
```

In this manner, the applicative style for Maybe supports a form of exceptional programming in which we can apply pure functions to arguments that may fail without the need to manage the propagation of failure ourselves, as this is taken care of automatically by the applicative machinery.

In summary, the applicative style for lists supports a form of non-deterministic programming in which we can apply pure functions to multi-valued arguments without the need to manage the selection of values or the propagation of failure, as this is taken care of by the applicative machinery.

```hs
instance Applicative IO where
   -- pure :: a -> IO a
   pure = return

   -- (<*>) :: IO (a -> b) -> IO a -> IO b
   mg <*> mx = do {g <- mg; x <- mx; return (g x)}


getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)
```

#### Applicative Laws

```hs
pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
```

All the applicative functors that we defined in the examples section satisfythe above laws. Moreover, each of these instances also satisfies the equation `fmap g x = pure g <*> x`, which shows how fmap can be defined in terms ofthe two applicative primitives. In fact, this latter law comes for free, by virtueof the fact that (as noted at the end of section 12.1) there is only one way tomake any given parameterised type into a functor, and hence any function withthe same polymorphic type as fmap must be equal to fmap.

> We conclude by noting that Haskell also provides an infix version of fmap, defined by `g <$> x = fmap g x`.

> applicative programming is about applying pure functions to effectful arguments.

### Monads

```hs
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                                 Nothing -> Nothing
                                 Just m -> safediv n m
```

Using this

```hs
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f = case mx of
              Nothing -> Nothing
              Just x -> f x
```

We can simplify the original block with:

```hs
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safediv n m
```

Expressions of the above form generalize to:

```hs
m1 >>= \x1 ->
m2 >>= \x2 ->
.
.
.
mn >>= \xn ->
f x1 x2 ... xn
```

But Haskell offers an even simpler manner of using them:

```hs
do x1 <- m1
   x2 <- m2
   .
   .
   .
   xn <- mn
   f x1 x2 ... xn
```

```hs
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m
```

More generally, the do notation is not specific to the types IO and Maybe, but can be used with any applicative type that forms a monad. In Haskell, the concept of a monad is captured by the following built-in declaration:

```hs
class Applicative m => Monad m where
   return :: a -> m a
   (>>=) :: m a -> (a -> m b) -> m b

   return = pure
```

So a monad is:

- An applicative type `m`
- That supports
   - `return`
   - `>>=` of the specified types

#### Examples

Lists can be turned into monads:

```hs
instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b]
   xs >>= f = [y | x <- xs, y <- f x]

-- Now we can:
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)
```

#### Monad Derivation

See file `12.hs` for the derivation of monads and an example with trees.

#### Generic Functions

Generic functions for Monads can be found in `Control.Monad`:

```hs
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = do y <- f x
                   ys <- mapM f xs
                   return (y:ys)
```

#### Monad Laws

```hs
return x >>= f   = f x
mx >>= return    = mx
(mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))
```

Together, these two equations state, modulo the fact that the second argument to >>= involves a binding operation, that return is the identity for the >>= operator.

## 13. Monadic Parsing

A parser is a program that takes a string of characters as input, and produces some form of tree that makes the syntactic structure of the string explicit.

The key difference is thata parser also has the possibility to fail by returning a list of results, whereas astate transformer always returns a single result. In this manner, a parser can be viewed as a generalised form of state transformer.

> This is why I hate academia: this is a real phrase from the book (page 196):
>
> In turn, <*> applies a parser that returns a function to a parser that returns an argument to give a parser that returns the result of applying the function to the argument, and only succeeds if all the components succeed.

## Making Choices

Making a choice between two alternatives isn’t specific to parsers, but can be generalised to a range of applicative types. This concept is captured by the following class declaration in the library Control.Applicative:

```hs
class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a
```

## 14. Foldables and Friends

In mathematics, a monoid is a set together with an associative operator thatcombines two elements from the set, and an identity element for the operator.For example, the set of integers forms a monoid with the operator given by addition and the identity element by the value zero.

```hs
class Monoid a where
   mempty :: a
   mappend :: a -> a -> a

   mconcat :: [a] -> a
   mconcat = foldr mappend mempty
```

That is, for a type a to be an instance of the class Monoid, it must support avalue mempty and a function mappend of the specified types, which respectivelyplay the role of the identity element and the operator for the monoid. In practice,the function mappend is often written as an infix operator by enclosing its name in single back quotes, as in `x ‘mappend‘ y`.

> The naming choice is unfortunate. All that is required is respecting the Monoid laws.

However, multiple instance declarations of the same type for the same class arenot permitted in Haskell, so attempting to declare two separate instances forMonoid Int in this manner will result in an error. The solution is to introduce special-purpose wrapper types for each of the two instances.

We conclude this section by noting that the library also provides an infix version of mappend, defined by `x <> y = x ‘mappend‘ y`, which allows monoid expressions to be written more concisely, as in `x <> y <> z`.

### Foldables

One of the primary applications of monoids in Haskell is to combine all the values in a data structure to give a single value.

```hs
fold :: Monoid a => [a] -> a
fold []     = mempty
fold (x:xs) = x `mappend` fold xs

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

fold :: Monoid a => Tree a -> a
fold (Leaf x)   = x
fold (Node l r) = fold l `mappend` fold r
```

`Data.Foldable`:

```hs
class Foldable t where
   fold    :: Monoid a => t a -> a
   foldMap :: Monoid b => (a -> b) -> t a -> b
   foldr   :: (a -> b -> b) -> b -> t a -> b
   foldl   :: (a -> b -> a) -> a -> t b -> a
```

1. Why are there so many functions in the class? In particular, one mightask why additional primitives such as null, length, and so on are provided asmethods in the Foldable class, rather than as definitions in the foldable library.The reason is to allow the default definitions to be overridden if required, which would not be possible if they were defined as top-level functions.
1. What do we need to define manually? The minimal complete definition foran instance of the Foldable class is to define either foldMap or foldr, as allother functions in the class can be derived from either of these two using thedefault definitions and the instance for lists. As we have already seen with lists and trees, it is often simplest to define the function foldMap.
1. What about efficiency? For many applications using the default definitionsthat are provided in the class will suffice, but if greater efficiency is requiredthese can be overridden, as noted above. In practice, the GHC system uses moreefficient default definitions than the simple versions we have presented, but these are functionally equivalent to our simpler versions.

We conclude this section by noting that GHC automatically imports the li-brary Data.Foldable, but currently hides the fold and toList methods of theclass. For this reason, we generally prefer to explicitly import Data.Foldablewhen programming with foldable types, rather than relying on the cut-down version that is automatically provided.

### Generic Functions

```hs
average :: [Int] -> Int
average ns = sum ns ‘div‘ length ns

-- Now it can be applied to both lists and trees
average :: Foldable t => t Int -> Int
average ns = sum ns ‘div‘ length ns
```

### Traversables

```hs
traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse g []     = pure []
traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs

-- Example
dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing
-- > traverse dec [1,2,3]
-- Just [0,1,2]
```

Traversable Types:

```hs
class (Functor t, Foldable t) => Traversable t where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```

The requirement that t is a functor reflects the fact thattraversables generalise the idea of mapping, and are hence expected to supportthe fmap primitive. The requirement that t is foldable ensures that values in a traversable type can also be folded up if desired.

## 15. Lazy Evaluation

More formally, in Haskell any two different ways of evaluating the same expression will always produce the same final value, provided that they both terminate.

### Evaluation Strategies

An expression that has the form of a function applied to one or more arguments that can be ‘reduced’ by performing the application is called a reducible expression, or redex for short.

As indicated by the use of quotations marks in the preceding sentence, such reductions do not necessarily decrease the size of an expression, although in practice this is often the case.

Innermost evaluation can also be characterised in terms of how arguments are passed to functions. In particular, using this strategy ensures that arguments are always fully evaluated before functions are applied. That is, arguments are passed by value.

In terms of how arguments are passed to functions, using outermost evaluation allows functions to be applied before their arguments are evaluated. For this reason, we say that arguments are passed by name.

We conclude this section by noting that many built-in functions require their arguments to be evaluated before being applied, even when using outermost evaluation. For example, as illustrated in the calculation above, built-in arithmetic operators such as * and + cannot be applied until their two arguments have beenevaluated to numbers. Functions with this property are called *strict*, and will be discussed in further detail at the end of this chapter.

### Lambda Expressions

Currying alters the sequence of reductions.

Note that in Haskell, the selection of redexes within the bodies lambda ex-pressions is prohibited. The rationale for not ‘reducing under lambdas’ is thatfunctions are viewed as black boxes that we are not permitted to look inside.More formally, the only operation that can be performed on a function is thatof applying it to an argument. As such, reduction within the body of a functionis only permitted once the function has been applied.

Using innermost and outermost evaluation, but not within lambda expressions, is normally referred to as call-by-value and call-by-name evaluation, respectively.

More generally, we have the following important property: if there exists any evaluation sequence that ter- minates for a given expression, then call-by-name evaluation will also terminate for this expression, and produce the same final result.

In summary, call-by-name evaluation is preferable to call-by-value for the pur- pose of ensuring that evaluation terminates as often as possible.

> Call-by-name vs Call-by-value

*The use of call-by-name evaluation in conjunction with sharing is known aslazy evaluation. This is the evaluation strategy that is used in Haskell, as a resultof which Haskell is known as a lazy programming language. Being based uponcall-by-name evaluation, lazy evaluation has the property that it ensures thatevaluation terminates as often as possible. Moreover, using sharing ensures thatlazy evaluation never requires more steps than call-by-value evaluation. The useof the term ‘lazy’ will be explained in the next section.*

### Infinite Structures

Using this idea, we now see that under lazy evaluation ones is not an infinitelist as such, but rather a potentially infinite list, which is only evaluated asmuch as required by the context. This idea is not restricted to lists, but appliesequally to any form of data structure in Haskell. For example, infinite trees are considered in the exercises for this chapter.

### Modular Programming

> **Lazy evaluation also allows us to separate control from data in our computations.**

That is, the data is only evaluated as much as required by the control, and these two parts take it in turn to perform reductions. Without lazy evaluation, the control and data parts would need to be combined in the form of a single function that produces a list of n identical elements.

> **By freeing the generation of prime numbers from the constraint of finiteness, we have obtained a modular program on which different control parts can be used in different situations.**

### Strict Application

Haskell uses lazy evaluation by default, but also provides a special strict versionof function application, written as `$!`, which can sometimes be useful. Informally,an expression of the form `f $! x` behaves in the same way as the normal func-tional application f x, except that the top-level of evaluation of the argument expression x is forced before the function f is applied.

When used with a curried function with multiple arguments, strict application can be used to force top-level evaluation of any combination of arguments. For example, if f is a curried function with two arguments, an application of the form f x y can be modified to have three different behaviours:

- (f $! x) y forces top-level evaluation of x
- (f x) $! y forces top-level evaluation of y
- (f $! x) $! y forces top-level evaluation of x and y

In Haskell, strict application is mainly used to improve the space performance of programs.

## 16. Reasoning About Programs

Patterns that do not rely on the order in which they are matchedare called non-overlapping. In order to simplify the process of reasoning aboutprograms, it is good practice to use non-overlapping patterns whenever possiblewhen defining functions. For example, most of the functions in the standardprelude given in appendix B are defined in this manner.

Now suppose we want to prove that some property, p say, holds for all (finite)natural numbers. Then the principle of induction states that it is sufficient toshow that p holds for Zero, called the base case, and that p is preserved by Succ,called the inductive case. More precisely, in the inductive case one is required toshow that if the property p holds for any natural number n, called the induction hypothesis, then it also holds for Succ n.

> The chapter remarks have quite some interesting resources.

## 17. Calculating Compilers


