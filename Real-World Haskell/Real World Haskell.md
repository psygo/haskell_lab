# Real World Haskell

---

**Table of Contents**

* [Preface](#preface)
* [1. Useful Commands](#1.-useful-commands)
* [2. Types and Functions](#2.-types-and-functions)
* [3. Defining Types, Streamlining Functions](#3.-defining-types,-streamlining-functions)
* [4. Functional Programming](#4.-functional-programming)
* [6. Typeclasses](#6.-typeclasses)
* [7. I/O](#7.-i/o)
* [8. Efficient File Processing, Regular Expressions, and Filename Matching](#8.-efficient-file-processing,-regular-expressions,-and-filename-matching)
* [9.](#9.)

---

## Preface

Additionally, although the focus of the language is squarely on writing statically typed programs, it is possible (though rarely seen) to write Haskell code in a dynamically typed manner.

## 1. Useful Commands

1. `:set prompt ghci>`
1. `info: <function>`
    - Will show associativity of the operator and its priority.
1. Missing endpoints in enumeration:
    ```hs
    ghci> [1,4..15]
    [1,4,7,10,13]
    ```
1. You might be tempted to try writing `[1,2]:3` to add an element to the end of a list, but
ghci will reject this with an error message, because the first argument of (:) must be
an element, and the second must be a list.
1. More basic is the (:) operator, which adds an element to the front of a list (this is
pronounced “cons” [short for “construct”])
1. Haskell makes a distinction between single characters and text strings. A single character
is enclosed in single quotes. (`'a':"bc"`)
1. Make ghci print type information as well with: `:set +t`. Or `ghci> :unset +t`
    - What the +t does is tell ghci to print the type of an expression after the expression. That cryptic it in the output can be very useful: it’s actually the name of a special variable, in which ghci stores the result of the last expression we evaluated.
    - That it variable is a handy ghci shortcut. It lets us use the result of the expression we just evaluated in a new expression
1. Rational numbers don’t look quite the same as integers. To construct a rational number,
we use the (%) operator. The numerator is on the left, the denominator on the right:
    ```hs
    ghci> :m +Data.Ratio
    ghci> 11 % 29
    11%29
    it :: Ratio Integer
    ```
1. `:?` on GHCi to get some help.

## 2. Types and Functions

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

1. Pure code is inherently modular: every function is selfcontained and has a well-defined interface.

1. A nonobvious consequence of purity being the default is that working with impure code becomes easier. Haskell encourages a style of programming in which we separate code that must have side effects from code that doesn’t need side effects. In this style, impure code tends to be simple, with the “heavy lifting” performed in pure code.

## 3. Defining Types, Streamlining Functions

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

## 4. Functional Programming

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

## 6. Typeclasses

- Without typeclasses, such as `==`: we have to use a function with a differentname for every different type that we want to be able to compare.

- Typeclasses define a set of functions that can have different implementations dependingon the type of data they are given. Typeclasses may look like the objects of object-oriented programming, but they are truly quite different

- While it is possible to build sophisticated parsers using the Read type-class, many people find it easier to do so using Parsec, and rely onRead only for simpler tasks.

- Note that there are also many more numeric types available for specific purposes such as interfacing to C.

- Notice that the sort order for Color was based on the order in which the constructorswere defined.

- if  you  defined  a  typedata MyType = MyType (Int -> Bool), the compiler will not be able to derive an instanceof Show because it doesn’t know how to render a function. We will get a compilationerror in such a situation.

- Relaxing a rule from GHC: `{-# LANGUAGE TypeSynonymInstances #-}`.

- Normally, we cannot write an instance of a typeclass for a specialized version of apolymorphic type. The [Char] type is the polymorphic type [a] specialized to the typeChar. We are thus prohibited from declaring [Char] to be an instance of a typeclass.This is highly inconvenient, since strings are ubiquitous in real code.
    - `TypeSynonymInstances`, `OverlappingInstances`

- The newtype keyword exists to give an existing type a new identity, and it has morerestrictions on its uses than the data keyword. Specifically, a newtype can have only onevalue constructor, which must have exactly one field.
    - The compiler treats UniqueID as a different type from Int. As a user of aUniqueID, we know only that we have a unique identifier; we cannot seethat it is implemented as an Int.

- Beyond this, there’s another important difference between data and newtype. A typecreated with the data keyword has a bookkeeping cost at runtime, for example, in orderto track which constructor created a value. A newtype value, on the other hand, canhave only one constructor and so does not need this overhead. This makes it morespace- and time-efficient at runtime.

- Because a newtype’s constructor is used only at compile time and does not even existat runtime, pattern matching on undefined behaves differently for types defined usingnewtype than for those that use data.

- When we don’t export a type’s data constructor, clients of our library can only use thefunctions we provide to construct and deconstruct values of that type. This gives us,the library authors, the liberty to change our internal representation if we need to.

- The monomorphism restriction to which the error message refers is a part of the Haskell98 standard. Monomorphism is simply the opposite of polymorphism: it indicates thatan expression has exactly one type. The restriction lies in the fact that Haskell sometimesforces a declaration to be less polymorphic than we would expect.

## 7. I/O

- Anything that is type IO something is an I/O action. You can store it and nothing willhappen. I could say writefoo = putStrLn "foo" and nothing happens right then. But ifI later use writefoo in the middle of another I/O action, the writefoo action will beexecuted when its parent action is executed—I/O actions can be glued together to formbigger I/O actions. The () is an empty tuple (pronounced “unit”), indicating that thereis no return value from putStrLn. This is similar to void in Java or C.
    - Produce an effect when performed, but not when evaluated.
    - The type of getLine may look strange to you. It looks like a value, rather than a function.And in fact, that is one way to look at it: getLine is storing an I/O action. When thataction is performed, you get a String. The <- operator is used to “pull out” the resultfrom performing an I/O action and store it in a variable.

- Notice that return call. This is not really the same as return in C or Python. In thoselanguages, return is used to terminate execution of the current function immediately,and to return a value to the caller. In Haskell, return is the opposite of <-. That is,return takes a pure value and wraps it inside IO.

- After you write out 5 bytes, your positionwill be 5, and so on. hTell takes a Handle and returns an IO Integer with your position.
    - The companion to hTell is `hSeek`.
    - Not all Handles are seekable. A Handle usually corresponds to a file, but it can alsocorrespond to other things such as network connections, tape drives, or terminals. Youcan use hIsSeekable to see if a given Handle is seekable.
    - `$ echo John|runghc callingpure.hs`
    - While you could craft a way to manually open files with unique names, the details ofdoing this in a secure way differ from platform to platform.

- Lazy IO
    - The String it returns is evaluated lazily. At the momentyou call hGetContents, nothing is actually read. Data is only read from the Handle as theelements (characters) of the list are processed. As elements of the String are no longerused, Haskell’s garbage collector automatically frees that memory. All of this happenscompletely transparently to you. And since you have what looks like (and, really, is) apure String, you can pass it to pure (non-IO) code.
    - you can think of the String between readFile and writeFile as a pipe linkingthe two. Data goes in one end, is transformed some way, and flows back out the other.

- Every statement, except let, in a do block must yield anI/O action that will be executed.

- Also, functionsthat end with an underscore typically discard their result.

- Why a mapM when we already have map? Because map is a pure function that returns alist. It doesn’t—and can’t—actually execute actions directly. mapM is a utility that livesin the IO monad and thus can actually execute the actions.

- The >> operator sequences two actions together: the first action is performed, and thenthe second. The result of the computation is the result of the second action

- Many command-line programs are interested in the parameters passed on the com-mand line. System.Environment.getArgs returns IO [String] listing each argument.This is the same as argv in C, starting with argv[1]. The program name (argv[0] in C)is available from System.Environment.getProgName.

## 8. Efficient File Processing, Regular Expressions, and Filename Matching

- The bytestring library provides a fast, cheap alternative to the String type. Code writ-ten with bytestring can often match or exceed the performance and memory footprintof C, while maintaining Haskell’s expressivity and conciseness.

- We import the ByteString modules using Haskell’s qualified import syntax, the importqualified that we just saw. This lets us refer to a module with a name of our choosing.
    - Whether or not we use qualified imports, we can always use the entire name of a moduleto  identify  something  unambiguously.  Both Data.ByteString.Lazy.length  andL.length, for instance, identify the same function, as do Prelude.sum and sum.

- Other Haskell regexp packages are available for download from Hackage. Some providebetter performance than the current POSIX engine (e.g., regex-tdfa); others providethe Perl-style matching that most programmers are now familiar with (e.g., regex-pcre). All follow the standard API that we have covered in this section.

## 9.
