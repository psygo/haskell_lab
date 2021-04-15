# QuickCheck

## Main Article

### 1. Introduction

Testing is by far the most commonly used approach to ensuring software quality. It is also very labour intensive, accounting for up to 50% of the cost of software development.

We have designed a simple domain-specic language of testable specifications which the tester uses to dene expected properties of the functions under test.

The specification language is embedded in Haskell using the class system. Properties are normally written in the same module as thefunctions they test, where they serve also as checkable documentation of the behaviour of the code.

Random testing is most e ective whenthe distribution of test data follows that of actual data, but when testing reuseable code units as opposed to whole systems this is not possible, since the distribution of actual data in all subsequent reuses is not known. A uniform distribution is often used instead, but for data drawn from infinite sets this is not even meaningful { how would onechoose a random closed lambda-term with a uniform distribution,for example? We have chosen to put distribution under the human tester's control, by defining a test data generation language (also embedded in Haskell), and a way to observe the distribution of test cases.

> QuickCheck only works well with finite data.

### 2. Defining Properties

In fact the programmer must provide a little more information: the function quickCheck is actually overloaded, inorder to be able handle laws with a varying number of variables, and the overloading cannot be resolved if the law itself has a polymorphic type, as in these examples. Thus the programmer must specify a fixed type at which the law is to be tested.

> `+` is associative for the type `Int` but not for `Double`!

QuickCheck can also be used for function composition testing, though it's more difficult to find out why, if the test fails.

```hs
prop_MaxLe x y = x <= y ==> max x y == y
```

Be careful with the distribution of the data. It might be severely skewed. Use `classify`, `orderedList` and `collect` to create better distributions and analyze them.

We can reformulate the property as a logically equivalent one, by using the fact that two infinite lists are equal if all infinite initialsegments are equal.

### 3. Defining Generators

The way we generate random test data of course dependson the type. Therefore, we have introduced a type classArbitrary, of which a type is an instance if we can generate arbitrary elements in it.

```hs
class Arbitrary a where
  arbitrary :: Gen a

newtype Gen a = Gen (Rand -> a)
```

### 5. Some Case Studies

#### 5.2 Circuit Properties

Lava [3, 7] is a tool to describe, simulate and formally verify hardware. Lava is a so-called embedded language, which means that the circuit descriptions and properties are all expressed in an existing programming language, in this case Haskell.

### 6. Discussion

#### 6.1. On Random Testing

In 1984, Duran and Ntafos compared the fault detection probability of random testing with partition testing, and discovered that the differences in effectiveness were small [9].

For small programs in particular, it is likely that random test cases will indeed exercise all paths, for example, so that test coverage is likely to be good by any measure.

So even when QuickCheck is used to test a large program, we always test a small part at a time. Therefore we may expect random testing to work particularly well.

#### 6.2. Correctness Criteria

The problem of determining whether a test is passed or not is known as the oracle problem.

### 6.6. Some Reflections

Checking and specification are different things:

> While it is open to the programmer to do this anyway, few really do, perhaps because there is little short term payoff, and perhaps because a specification is of less value if there is no check at all that it corresponds to the implemented program.
