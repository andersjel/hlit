Example
=======

Should print 42: `@ product [7, 3, 2] :: Int`.

Here we bring something into scope

> testExpression = show [1..10]

that we can use inline by typing `@testExpression`.
We can refer to `@stuff` before defining it.

> stuff :: String
> stuff = "an expression"
