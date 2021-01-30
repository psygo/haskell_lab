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