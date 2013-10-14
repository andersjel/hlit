Example
=======

Should print 42: `@ product [7, 3, 2] :: Int`.

Here we bring something into scope

> testExpression = show [1..10]

that we can use inline by typing `@testExpression`.
We can refer to `@stuff` before defining it.

> stuff :: String
> stuff = "an expression"

Rendering of *Maybes* and *Eithers* are designed to be
usefull when writing prose, thus the constructors Just and 
Right are omitted.

-------------------------------------- ------------------------------------
Code                                   Result
-------------------------------------- ------------------------------------
`'@Just 42 :: Maybe Int'`              `@Just 42 :: Maybe Int`

`'@Nothing :: Maybe Int'`              `@Nothing :: Maybe Int`

`'@Right 42 :: Either String Int'`     `@Right 42 :: Either String Int`

`'@Left "msg" :: Either String Int'`   `@Left "msg" :: Either String Int`
-------------------------------------- ------------------------------------
