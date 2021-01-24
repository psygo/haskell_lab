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