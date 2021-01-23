# 2. Types and Functions

1. It’s a truism within the Haskell community that once code compiles, it’s more likely to work correctly than in other languages. (Perhaps a more realistic way of putting this is that Haskell code often has fewer trivial bugs.)

1. A narrower type, Float, also exists, but its use is discouraged; Haskell compiler writers concentrate more on making Double efficient, so Float is much slower.

1. If we omit the :: and the type that follows, a Haskell compiler will infer the type of the expression:
    ```hs
    ghci> 'a' :: Char
    'a'
    ```

1. Function application has higher precedence than using operators.

1. A composite data type is constructed from other types. The most common composite data types in Haskell are lists and tuples.

1. There’s a special type, (), that acts as a tuple of zero elements. This type has only one value, which is also written (). Both the type and the value are usually pronounced “unit.” If you are familiar with C, () is somewhat similar to void.

1. Haskell doesn’t have a notion of a one-element tuple.

1. We often use tuples to return multiple values from a function. We can also use them any time we need a fixed-size collection of values, if the circumstances don’t require a custom container type.

1. In Haskell,function application is left-associative. This is best illustrated by example: the expression a b c d is equivalent to (((a b) c) d).

1. Side effects are essentially invisible inputs to, or outputs from, functions.

1. `ghci> :cd /tmp`

1. Haskell doesn’t have a return keyword, because a function is a single expression, not a sequence of statements. The value of the expression is the result of the function.

1. In Haskell, a variable provides a way to give a name to an expression. Once a variable is bound to (i.e., associated with) a particular expression, its value does not change: we can always use the name of the variable instead of writing out the expression, and we will get the same result either way.

1. In Haskell, indentation is important: it continues an existing definition, instead of starting a new one. Don’t omit the indentation! If we wish, we can write the entire expression on a single line.

1. The (||) operator isn’t built into the language; it’s an ordinary function.

1. In Haskell, the subexpression 1 + 2 is not reduced to the value 3. Instead, we create a “promise” that when the value of the expression isOdd (1 + 2) is needed, we’ll be able to compute it. The record that we use to track an unevaluated expression is referred to as a thunk. This is all that happens: we create a thunk and defer the actual evaluation until it’s really needed. If the result of this expression is never subsequently used, we will not compute its value at all.

1. Notice that as we return from each successive recursive application, none of them needs to evaluate the expression tail "bcd": the final result of evaluating the original expression is a thunk.

1. If a type contains type parameters, we say that it is a parameterized type, or a polymorphic type. If a function or value’s type contains type parameters, we call it polymorphic.

1. Haskell’s parametric polymorphism directly influenced the design of the generic facilities of the Java and C# languages.

1. In mainstream object-oriented languages, subtype polymorphism is more widespread than parametric polymorphism. The subclassing mechanisms of C++ and Java give them subtype polymorphism. A base class defines a set of behaviors that its subclasses can modify and extend. Since Haskell isn’t an object-oriented language, it doesn’t provide subtype polymorphism.

1. Also common is coercion polymorphism, which allows a value of one type to be implicitly converted into a value of another type. Many languages provide some form of coercion polymorphism; one example is automatic conversion between integers and floating-point numbers. Haskell deliberately avoids even this kind of simple automatic coercion.

1. On the type signature of `fst`: first of all, notice that its argument contains two type variables, a and b, signifying that the elements of the tuple **can** be of different types.

1. The arrows in type signatures are right associative.
    ```hs
    take :: Int -> ([a] -> [a])
    ```
    - From this, it looks like we ought to read the type signature as a function that takes one argument, an Int, and returns another function. This is correct, but it’s not easy to see what its consequences might be. We’ll