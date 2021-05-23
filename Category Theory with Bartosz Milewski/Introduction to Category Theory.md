# Introduction to Category Theory

Tags: Haskell, Category Theory, Math

[Category Theory 1.1: Motivation and Philosophy - YouTube](https://www.youtube.com/watch?v=I8LbkfSSR58&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

## 1.1. Motivation and Philosophy

- The main problem with OO is that it abstracts over mutation and shareing data: data races. You're not supposed to abstract over data races.
    - Locks don't compose either (in Java, every object has locks)
- Template Programming is basically an adaptation of imperative languages to something like Haskell.
- Category Theory is the higher level language above Haskell and other programming languages.
    - It's meta-math.
- Category theory is more like epistemology (how we reason about things) than ontology (what things are).

## 1.2. What is a category?

We want to keep:

- Abstraction
- Composition
- Identity

Composition and identity is basically what a category theory.

*A category is a bunch of objects.*

A morphism is something that goes from one object to another. You can even have arrows from an object to itself.

Different composition tables contain different categories.

A category needs:

- Composition
- Identity
- Associativity

> A group is a monoid that also has an inverse.

Categories don't take *time* into account, hence laziness.

Categories don't look inside sets, the set is an object.

## 2.1. Functions and Epimorphisms

A function is pure if you can *memoize* it, i.e., you can turn it into a table. (Infinity can still be tabulated, it's a problem of resources.)

A subset of the cartesian product is a relation. And functions are directed relations. And we need total functions (all the elements are mapped to something). (Domain, Co-domain, Image)

Usually, since the image is typically a subset of the co-domain, functions are not invertible.

Isomorphism for any category (invertibility):

```hs
f :: a -> b
g :: b -> a

g . f = idA
f . g = idB
```

> A set in the domain being mapped to a single point in the co-domain constitutes a *fiber*.

- Injective (monoc, monomorphism): does not collapse, no shrinking
    - Pre-composition
- Surjective (epic, epimorphism): covers the whole thing
    - The definition of epimorphism is a little bit more nuanced. (See the end of lecture 2.1)
        - Post-composition
- Bijective: injective + surjective
    - Epi + Mono != Isomorphism

> In category theory, things are usually defined with respect to everything else.

> Category Theory prefers Greek over Latin.

In Haskell, the empty set is the type `Void`, which has no way of being constructed. 

Is there a function `absurd :: Void -> a`? Yes, it follows from logic, where you cannot prove anything that is `False`, it is false by definition. `Void` is useful also for definition is canon identity `id_Void :: Void -> Void`.

The empty set is the `()` in Haskell, which is also called the unit for sets. The `()` unit corresponds to `True`. (`unit :: a -> ()`) (`one :: () -> Int`)

> A function that goes from `Bool` to `Bool` is called a predicate.

## 3.1. Examples of Categories, Orders, Monoids

### Examples

If there are no objects, there are no arrows. But everything is satisfied by definition basically. That's the *0* category. It's important for categories of all categories.

The *1* category is basically the internal category, with identity arrows.

You can have 2 objects with identities, 2 objects with identities and a function, etc.

In general, you can start with a graph. But not all graphs are categories. We have to:

- Add identity arrows
- For every pair of composable arrows, we need to add composition arrows.
- Associativity simplifies the amount of needed arrows.

This process is called free construction.

16:00 Ordering is a very special category. `<=` has automatic identity and composability. Thin category === Pre-order

> Pre-order is an example of something that's epi- and mono-morphism but doesn't have an inversion.

Set of arrows: *hom-set*

Any category with just one object is called a monoid. However it's also defined for set and group theory. In sets and groups, you still need a unit, a binary operator and associativity (not commutativity). The hom-set of a monoid is `M(m,m)`.

## 3.2. Kleisli Category

Functions that log stuff are not pure nor local. They are basically impossible to memoize.

A Kleisli category is an embellishment, it's also a monad.

```hs
-- Example:
f :: a -> (A, String)
```

## 4.1. Terminal and Initial Objects

An example of *terminal object* is the empty set (`()`). From every set, we can always go to the unit, terminal object, in this case.

No matter what path you have to the terminal object, you can always shrink it to one arrow.

Not all categories have terminal objects.

Initial objects go have only out-going arrows.

Terminal objects are unique up to isomorphism.

40:30 In isomorphism, `g . f = id` must be true because there must be an arrow going from `a` to `a` because it ends up being both an initial and terminal object.


## 4.2. Products

For every category, we can create a new one which is composed of reversing the arrows on the original category.

33:30 The product of `a` and `b`:

```hs
p :: c -> a
q :: c -> b
```

Product is the cleanest projection for `p`, `q`, and `c`. The product is said to be `(p,q)`.

In Haskell, `p` and `q` could be viewed as the `fst` and `snd` "getters".

## 5.1. Coproducts, Sum Types

The dual of the product is created on the co-category.

If

```hs
i, i' :: a -> c
j, j' :: b -> c

i' = m . i
j' = m . j
```

Then `i` and `j` are the purest forms. The co-product is the pair of injections `(i,j)`. The co-product unionizes the sets of `a` and `b`.

If 2 equal sets are part of the co-product, it's a *tagged union* or *variant*.

This co-product is basically the `Either` data type:

```hs
data Either a b = Left a | Right b
```

## 5.2. Algebraic Data Types

Are the tuples `(a,b)` and `(b,a)` the same? No, but they are isomorphic to swapping, and they contain the same information.

- Associative: `((a,b),c) = (a,(b,c))`
- `(a,()) ~ a`
- `Either a b ~ Either b a`
- `Either a Void ~ a`
- `(a,Void) ~ Void` (`a * 0 = 0`)
- `(a, Either b c) ~ Either (a,b) (a,c)` <-> `a * (b + c) = a * b + a * c

Multiplication + Addition + Inverse of Addition = Ring. A Ring without an inverse is called a Rig or Semi-Ring (what's the inverse of integer as a type? It doesn't exist).

```hs
data Maybe a = Nothing | Just a
-- Is equivalent to `Either () a`
-- Which is equivalent to `1 + a`
```

```hs
-- l(a) = 1 + a * l(a)
-- l(a) - a * l(a) = 1
-- l(a) (1 - a) = 1
-- l(a) = 1 / (1 - a)

-- That's equivalent to:
data List a = Nil | Cons a (List a)

-- It's also the sum of geometric sequences (sum_n(a^n))

-- That's why these are called algebraic data types
```

## 6.1. Functors

We are interested in mappings that preserve structure. And it so happens that functions are mappings between sets, which have no structure &mdash; maybe that's why it's so difficult to implement sets in hardware, which has inherent structure.

A category with only the identity arrows describes a set, and it's called a discrete category.

A functor maps one category into another. It also maps morphisms and composition in the other category. And `F(g . f) = Fg . Ff`. Identity should also work.

If a functor is *injective*, we call it *faithful* on all hom-sets. If a functor is *surjective*, we call it *full*.

- The functor that collapses everything into a single object is called the *constant functor*.

Functors that map into their own category are called *endofunctors*, as in endoscopy.

```hs
-- In Haskell, we make it parametrically polymorphic, which is more restricted than category theory. This is one of the premises for the "Theorems for Free" paper.
f    :: a        -> b
fmap :: (a -> b) -> Maybe a -> Maybe b
-- You could also see it as `(a -> b) -> (Maybe a -> Maybe b)`
```

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  
data List a = Nil | Cons a (List a)
  
instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)
  
-- "Give me a double, and I'll give you a function of, say, bool to double"
type Reader r a = r -> a

fmap :: (a -> b) -> (r -> a) -> (r -> b)
fmap = (.) -- r -> a -> b
```

## 7.1. Functoriality & Bifunctors

A category of categories in which functors are the morphisms is another category called *Cat*.

Composition of functors example:

```hs
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs
```

The tuple `(,)` can be viewed as the product of 2 categories, with for example `(f,g)` as internal morphisms.

A *bifunctor* is a functor from a product category.

```hs
class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')
  -- This works for `Either a b` too.
```

## 7.2. Monoidal Categories, Functoriality of ADTs, Profunctors

A category with a unit is a monoidal category.

A monoidal category has a tensor product x 1.

```hs
data Const c a = Const c -- contructor and data names are not the same to the compiler

instance Functor (Const c) where
  -- fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c
  
data Identity a = Identity a

instance Functor (Identity a) where
  fmap f (Identity a) = Identity (f a)
  
-- There's an extension `{-# LANGUAGE DeriveFunctor #-}` that lets you have the compiler define the functors automatically for you. This comes as a derivation from the "Theorems for Free" paper.

data Maybe a = Nothing | Just a
               deriving Functor
```

A *Contravariant Functor* lets you revert paths &mdash; it should probably have been called opposite functor, but this had been discovered in tensors first &mdash;, in the opposite category:

```hs
class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)
```

A *Profunctor* is what of type `C_op x C -> C`:

```hs
class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
```

## 8.1. Function Objects, Exponentials

```hs
curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f = \a -> (\b -> f (a,b))

uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry f = \(a,b) -> (f a) b
```

Synonyms: `a -> b ~ b^a | Bool -> Int ~ Int^Bool`

The terminal product it the 0th power, which terminates in a unit in many cases.

```math
a^0 = 1
Void -> a ~ ()

1^a = 1
a -> () ~ ()

a^1 = a
() -> a ~ a

a^(b+c) = a^b x a^c
Either b c -> a
(b -> a, c -> a)

(a^b)^c = a^(bxc)
c -> (b -> a) ~ (b,c) -> a
Currying

(axb)^c = a^c x b^c
c -> (a,b) ~ (c -> a, c -> b)
```

If we can construct an object of a proposition type, then it's true.

Function type corresponds to implication (`=>`).

| true     | false   | a and b   | a or b     | a => b |
| ()       | Void    | (a,b)     | Either a b | a -> b |
| terminal | initial | a x b     | a | b      | b^a    |

a => b and a -> b ~ ((a => b), a) -> b 

Curry-Howard-Lambek: Cartesian-closed model is also a model for logic and type theory.

## 9.1. Natural Transformations

The 3 most important definitions in category theory:

- Category: structure
- Functor: embedding one category into another
- Natural Transformations

Natural transformations are defined as mappings between functors. This is a commuting diagram between categories.

```hs
alpha . fmap f = fmap f . alpha -- 30:30: In Haskell, this is stronger than the categorical definition

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x
```

This is useful for optimizing code, because then you can systematically discover which function to apply first. A natural transformation repackages a container.

Since ADTs are functors, functions from one ADT to another are natural transformations.

## 9.2. Bicategories

Functors between categories notation: [C,D] or D^C

The definition of 2-category involves vertical and horizontal composition. A bicategory is a lax 2-category with "up-to-isomorphism".

## 10.1. Monads

> The animal-eating-function and composition is really funny.

Monads, like functions, are about composition.

It replaces the `.` with the fish operator (Kleisli arrow) (`>=>`) (the Kleisli arrow is usually not used because it's harder to read, like the `.`). And `id` ~ `return`. `return` helps us develop something in imperative style, which isn't bad, as long as it is controlled.

```hs
-- The true inspiration for the monadic bind is the Kleisli arrow, which is easier to read
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \a -> let mb = f a
                in  mb >>= g
(>>=) :: m b -> (b -> m c) -> m c
mb >>= f = join (fmap f ma)
join :: m (m a) -> m a

class Functor m => Monad m where
  join   :: m (m a) -> m a
  return :: a -> m a

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

Examples:

```hs
-- join :: Maybe (Maybe a) -> Maybe a
-- join (Just (Just a)) = Just a
-- join _               = Nothing

instance Maybe where
  Nothing >>= f = Nothing
  Just a  >>= f = f a

  return a = Just a
    
    
newtype State s a = State (s -> (a,s))

instance State where
  -- (a,s) -> (b,s) ~ a -> (s -> (b,s)) (Currying, now it looks more like the Kleisli arrow)
```

## 10.2. Monoid in the Category of Endofunctors

Monad is just a monoid in the category of endofunctors.

