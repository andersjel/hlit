Example
=======

Should print 42: `@ prod [7, 3, 2]`.

Here we bring something into scope

> testExpression = show [1..10]

that we can use inline by typing `@testExpression`.
We can use `@stuff` before it is defined.

> stuff :: String
> stuff = "things"
