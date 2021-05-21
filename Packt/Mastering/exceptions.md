# Exceptions

[gelisam/mastering-haskell: The slides for my Packt course, "Mastering Haskell".](https://github.com/gelisam/mastering-haskell)

Exceptions are a controversial topic in Haskell, because they are not tracked by the typing system. So they can be thrown even inside pure functions. It's not possible to catch exceptions in pure code though.

The runtime system needs to throw exceptions, so it makes sense to expose it to the user as another feature anyway.

`throwIO` is one way of throwing exceptions.

## Functional Reactive Programming (FRP)

- Video 49
- Video 53: animations

## Parallelism

Parallelism: how the program is written
Concurrency: how the program is executed
