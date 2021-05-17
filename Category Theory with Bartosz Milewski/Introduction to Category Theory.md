# Introduction to Category Theory

Tags: Haskell, Category Theory, Math

[Category Theory 1.1: Motivation and Philosophy - YouTube](https://www.youtube.com/watch?v=I8LbkfSSR58&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_)

- The main problem with OO is that it abstracts over mutation and shareing data: data races. You're not supposed to abstract over data races.
    - Locks don't compose either (in Java, every object has locks)
- Template Programming is basically an adaptation of imperative languages to something like Haskell.
- Category Theory is the higher level language above Haskell and other programming languages.
    - It's meta-math.
- Category theory is more like epistemology (how we reason about things) than ontology (what things are).

## What is a category?

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
