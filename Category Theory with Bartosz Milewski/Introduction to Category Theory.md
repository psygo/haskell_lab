# Introduction to Category Theory

Tags: Haskell, Category Theory, Math

[Category Theory 1.1: Motivation and Philosophy - YouTube](https://www.youtube.com/watch?v=I8LbkfSSR58&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

- The main problem with OO is that it abstracts over mutation and shareing data: data races. You're not supposed to abstract over data races.
    - Locks don't compose either (in Java, every object has locks)
- Template Programming is basically an adaptation of imperative languages to something like Haskell.
- Category Theory is the higher level language above Haskell and other programming languages.
    - It's meta-math.
- Category theory is more like epistemology (how we reason about things) than ontology (what things are).

## 1. What is a category?

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

## 2. Functions and Epimorphisms

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

## 3. Examples of Categories, Orders, Monoids

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

### Kleisli Category

Functions that log stuff are not pure nor local. They are basically impossible to memoize.

A Kleisli category is an embellishment, it's also a monad.

```hs
-- Example:
f :: a -> (A, String)
```

## 4. Terminal and Initial Objects

An example of *terminal object* is the empty set (`()`). From every set, we can always go to the unit, terminal object, in this case.

No matter what path you have to the terminal object, you can always shrink it to one arrow.

Not all categories have terminal objects.

Initial objects go have only out-going arrows.

Terminal objects are unique up to isomorphism.

40:30 In isomorphism, `g . f = id` must be true because there must be an arrow going from `a` to `a` because it ends up being both an initial and terminal object.


### 4.2. Products

For every category, we can create a new one which is composed of reversing the arrows on the original category.

33:30 The product of `a` and `b`:

```hs
p :: c -> a
q :: c -> b
```

Product is the cleanest projection for `p`, `q`, and `c`.

### 5.1. Coproducts, Sum Types

The dual of the product is created on the co-category.

