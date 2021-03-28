fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- From this article:
-- [Recursion: Fibonacci in Go?][fib_go]
-- [fib_go]: https://fanaro.io/articles/recursion_fibonacci_in_go/recursion_fibonacci_in_go.html
numberOfLiberties :: Int -> Int
numberOfLiberties 1 = 1
numberOfLiberties 2 = 2
numberOfLiberties ei = ei - 2 + (numberOfLiberties (ei-1))
