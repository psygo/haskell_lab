# Defining Types, Streamlining Functions

1. GHCi is capable of finding where you defined something:
    ```hs
    ghci> :info BookInfo
    data BookInfo = Book Int String [String]
    -- Defined at BookStore.hs:4:5-12
    instance Show BookInfo -- Defined at BookStore.hs:4:5-12
    ```

1. However, in Haskell, the names of types and values are independent of each other. We use a type constructor (i.e., the type’s name) only in a type declaration or a type signature. We use a value constructor only in actual code. Because these uses are distinct, there is no ambiguity if we give a type constructor and a value constructor the same name. Not only is it legal for a value constructor to have the same name as its type constructor, it’s normal.

1. The `type` keyword introduces a type synonym. The new name is on the left of the `=`, with the existing name on the right. The two names identify the same type, so type synonyms are purely for making code more readable.

1. The familiar Bool is the simplest common example of a category of type called an algebraic data type.
    ```hs
    data Bool = False | True
    ```
    - When a type has more than one value constructor, they are usually referred to as alternatives or cases.
    - Although the phrase “algebraic data type” is long, we’re being careful to avoid using the acronym “ADT,” which is already widely understood to stand for “abstract data type.” Since Haskell supports both algebraic and abstract data types, we’ll be explicit and avoid the acronym entirely.

1. The `(==)` operator requires its arguments to have the same type.

1. If we create a Shape value using the `Circle` constructor, the fact that we created a `Circle` is stored. When we later use a `Circle`, we can’t accidentally treat it as a `Square`.
    ```hs
    type Vector = (Double, Double)
    data Shape = Circle Vector Double
               | Poly [Vector]
    ```

1. Pattern Matching: A pattern lets us look inside a value and bind variables to the data it contains.
    - If the type has more than one value constructor, we need to be able to tell which value constructor was used to create the value.
    - If the value constructor has data components, we need to be able to extract those values.
    - As we already mentioned, a Haskell implementation checks patterns for matches in the order in which we specify them in our equation.
    - Because pattern matching acts as the inverse of construction, it’s sometimes referred to as deconstruction.
    - Patterns should be **exhaustive**.
        - For example, if we’re inspecting a list, we should have one equation that matches the non-empty constructor (:) and one that matches the empty-list constructor [].
        - GHC provides a helpful compilation option, `-fwarn-incompletepatterns`, that will cause it to print a warning during compilation if a sequence of patterns doesn’t match all of a type’s value constructors.
        - When we define a type using record syntax, it also changes the way the type’s values are printed.
            - The standard System.Time module makes good use of record syntax.

1. In Haskell, we don’t have an equivalent of `null`.

1. Haskell provides a standard function, error :: String -> a, that we can call when something has gone terribly wrong in our code.
    - It has a result type of a so that we can call it anywhere and it will always have the right type. However, it does not return a value like a normal function. Instead, it immediately aborts evaluation and prints the error message we give it.
    - This is the major weakness of using error: it doesn’t let our caller distinguish between a recoverable error and a problem so severe that it really should terminate our program.
        - If we want to indicate that an operation has failed, we can use the `Nothing` constructor. Otherwise, we wrap our value with the `Just` constructor.

1. Local Variables
    - Let us reemphasize our wording: a name in a `let` block is bound to an expression, not to a value. Because Haskell is a lazy language, the expression associated with a name won’t actually be evaluated until it’s needed.
    - It’s perfectly legal, but not exactly wise, to repeat a variable name in a nested `let` expression.
    - Shadowing can obviously lead to confusion and nasty bugs, so GHC has a helpful -fwarn-name-shadowing option.
    - The definitions in a `where` clause apply to the code that precedes it.
    - You’ll have noticed that Haskell’s syntax for defining a variable looks very similar to its syntax for defining a function. This symmetry is preserved in let and where blocks; we can define local functions just as easily as local variables.
        ```hs
        pluralise word counts = map plural counts
          where plural 0 = "no " ++ word ++ "s"
                plural 1 = "one " ++ word
                plural n = show n ++ " " ++ word ++ "s"
        ```

1. The Offside Rule
    - In Haskell, whitespace has meaning.
    - At the beginning of a source file, the first top-level declaration or definition can start in any column, and the Haskell compiler or interpreter remembers that indentation level. Every subsequent top-level declaration must have the same indentation.
    - If you use a Haskell-aware text editor (e.g., Emacs), it is probably already configured to use space characters for all whitespace when you edit Haskell source files. If your editor is not Haskell-aware, you should configure it to use only space characters.
        - The Haskell language standard requires implementations to use the Unix tab width convention.
    - We can use explicit structuring anywhere that we’d normally use layout.
    - Just remember that although the facility exists, explicit structuring is hardly ever actually used in Haskell programs.

1. The `case` Expression
    - The case construct lets us match patterns within an expression.
    - To express “here’s the expression to evaluate if none of the other patterns matches,” we just use the wild card pattern _ as the last in our list of patterns.
    - We refer to a pattern that always succeeds as irrefutable. Plain variable names and the wild card _ (underscore) are examples of irrefutable patterns.