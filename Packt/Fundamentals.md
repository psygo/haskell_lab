# Fundamentals

[Learning Path: Haskell: Functional Programming and Haskell | Udemy](https://www.udemy.com/course/learning-path-haskell-functional-programming-and-haskell/)

## Stack

Some useful commands:

- `stack new <project-name> <simple>` for creating new projects
- `stack build` builds the executable
- `stack setup` downloads even the compiler
- `stack exec` executes the compiled binary
- `stack ghc` compiles code
    - `stack ghc -- -o TraversalHS Traversal.hs`
    - `stack runghc Program.hs`
    
## GHCi

`:!` executes a shell command, e.g. `:!ls`.

Useful commands:

- `:kind`
- `:info`
- `:main`
- `:browse`

The `.ghc` file can be used to create configurations for GHCi. Ex.:

```ghci
-- This is will `cat` the file with `:catn <filename>`
:def catn \p -> return $ ":! catn\"" ++ p ++ "\""
```

### Debugging with GHCi

You can add breakpoints on functions with `:break`, e.g. `:break readInts`.

`_`s represent thunks.

- `:sprint` (simplified print) allows us to examine a variable without forcing it.
    - Use `:force` to force evaluation.
- `:continue` goes forward.
- `:list` will list the source code around our breakpoint.
- `:show breaks` lists breakpoints.
    - `:delete` for deleting breakpoints.
    - `:step` for single-stepping.
    - `:stack` for analyzing the stack.
- `:show bindings` shows the bindings.
- `:abandon` abandons the computation.

## Types

`Char` syntax &mdash; you can also concatenate them directly in a `String` &mdash;:

- `\0088`: decimal unicode
- `\x0068`: hexadecimal unicode
- `\o0130`: octal unicode

`newtype` also allows for typeclasses.
