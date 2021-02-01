# Functional Programming

1. Infix notation works even for data types.

1. The Data.List module is the “real” logical home of all standard list functions. The Prelude merely re-exports a large subset of the functions exported by Data.List. Several useful functions in Data.List are not re-exported by the standard Prelude.

1. Page 78: Several of the preceding functions behave poorly on empty lists, so be careful if you don’t know whether or not a list is empty.

1. We’ve already seen the definition of the list algebraic data type many times, and **we know that a list doesn’t store its own length explicitly**. Thus, the only way that length can operate is to walk the entire list.
    - Therefore, when we care only whether or not a list is empty, calling length isn’t a good strategy. It can potentially do a lot more work than we want, if the list we’re working with is finite. Since Haskell lets us easily create infinite lists, a careless use of length may even result in an infinite loop.
    - A more appropriate function to call here instead is `null`, which runs in constant time.

1. Partial and Total Functions
    - Functions that have only return values defined for a subset of valid inputs are called partial functions (calling error doesn’t qualify as returning a value!). We call functions that return valid results over their entire input domains total functions.
    - Some Haskell programmers go so far as to give partial functions names that begin with a prefix such as unsafe so that they can’t shoot themselves in the foot accidentally.
  
1. Handling the empty and nonempty cases separately, is a kind of approach called structural recursion.
    - Clearly, a recursive function would be at a huge disadvantage relative to a loop if it allocated memory for every recursive application—this would require linear space instead of constant space. However, functional language implementations detect uses of tail recursion and transform tail recursive calls to run in constant space; this is called tail call optimization (TCO).
        - Few imperative language implementations perform TCO; this is why using any kind of ambitiously functional style in an imperative language often leads to memory leaks and poor performance.

1. We’ve already matched on the nonempty constructor in the first equation that defines myMap. By elimination, the constructor in the second equation is necessarily the empty list constructor, so there’s no need to perform a match to see what its value really is.

1. The class of functions that we can express using foldr is called primitive recursive. A surprisingly large number of list manipulation functions are primitive recursive.
    - In fact, we can even write foldl using foldr!
    - As our extended treatment of folds should indicate, the foldr function is nearly as important a member of our list-programming toolbox as the more basic list functions we saw in “Working with Lists” on page 77. It can consume and produce a list incrementally, which makes it useful for writing lazy data-processing code.

1. To keep our initial discussion simple, we use foldl throughout most of this section. This is convenient for testing, but we will never use foldl in practice.
    - Not surprisingly, a thunk is more expensive to store than a single number, and the more complex the thunked expression, the more space it needs. For something cheap such as arithmetic, thunking an expression is more computationally expensive than evaluating it immediately. We thus end up paying both in space and in time.
    - On small expressions, foldl will work correctly but slowly, due to the thunking overhead that it incurs. We refer to this invisible thunking as a space leak, because our code is operating normally, but it is using far more memory than it should.
    - The Data.List module defines a function named `foldl'` that is similar to foldl, but does not build up thunks.
    - The article “A tutorial on the universality and expressiveness of fold” by Graham Hutton (http://www.cs.nott.ac.uk/~gmh/fold.pdf) is an excellent and in-depth tutorial that covers folds. It includes many examples of how to use simple, systematic calculation techniques to turn functions that use explicit recursion into folds.

1. Lambda Functions
    - This is followed by the function’s arguments (which can include patterns), and then an arrow (->) to introduce the function’s body.
    - Most importantly, while we can write a normal function using multiple clauses containing different patterns and guards, a lambda can have only a single clause in its definition.
    - We can’t write multiple clauses to define a lambda, we must be certain that any patterns we use will match.
        - Make sure your patterns can’t fail!

1. Partial Function Application and Currying
    - The implication here is very important. In Haskell, all functions take only one argument. While dropWhile looks like a function that takes two arguments, it is actually a function of one argument, which returns a function that takes one argument.
    - If we apply zip3 with just one argument, we get a function that accepts two arguments. No matter what arguments we supply to this compound function, its first argument will always be the fixed value we specified
    - **My summary**:
        - A function of 3 arguments is equivalent of a function of 1 argument applying another function of 2 arguments.
    - Haskell provides a handy notational shortcut to let us write a partially applied function in infix style. If we enclose an operator in parentheses, we can supply its left or right argument inside the parentheses to get a partially applied function. This kind of partial application is called a *section*.
        ```hs
        all (`elem` ['a'..'z']) "Frobozz"
        ```

1. As-patterns
    - The pattern `xs@(_:xs')` is called an as-pattern, and it means “bind the variable `xs` to the value that matches the right side of the `@` symbol.”
    - As-patterns have a more practical use than simple readability: they can help us to share data instead of copying it. In our definition of noAsPattern, when we match (x:xs), we construct a new copy of it in the body of our function. This causes us to allocate a new list node at runtime. That may be cheap, but it isn’t free. In contrast, when we defined suffixes, we reused the value xs that we matched with our as-pattern. Since we reuse an existing value, we avoid a little allocation.

1. Composition
    ```hs
    compose :: (b -> c) -> (a -> b) -> a -> c
    compose f g x = f (g x)

    -- Plugging functions into each other like this is so common that the Prelude provides function composition via the (.) operator
    suffixes5 = init . tails
    ```
    - The `(.)` operator isn’t a special piece of language syntax—it’s just a normal operator.

> **It is perfectly reasonable to skip this section until you encounter a space leak “in the wild.” Provided you use foldr if you are generating a list, and foldl' instead of foldl otherwise, space leaks are unlikely to bother you in practice for a while.**

1. Avoiding Space Leaks with `seq`
    - We refer to an expression that is not evaluated lazily as strict, so foldl' is a strict left fold. It bypasses Haskell’s usual nonstrict evaluation through the use of a special function named seq.
    ```hs
    :type seq
    seq :: a -> t -> t
    ```
    - It operates as follows: when a seq expression is evaluated, it forces its first argument to be evaluated, and then returns its second argument.
    - To have any effect, a seq expression must be the first thing evaluated in an expression.
    - To strictly evaluate several values, chain applications of seq together.
    - When evaluating an expression, seq stops as soon as it reaches a constructor. For simple types such as numbers, this means that it will evaluate them completely. Algebraic data types are a different story. Consider the value (1+2):(3+4):[]. If we apply seq to this, it will evaluate the (1+2) thunk. Since it will stop when it reaches the first (:) constructor, it will have no effect on the second thunk.
    - It is important to understand that seq isn’t free: it has to perform a check at runtime to see if an expression has been evaluated. Use it sparingly.