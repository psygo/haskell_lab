# 1. Useful Commands

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