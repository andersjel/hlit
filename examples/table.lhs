> import Text.Lit.Table

This is a very basic way to factor an integer

> import Data.List (find)
>
> factors n = case find isFactor [2..n-1] of
>     Just f  -> f : factors (n `div` f)
>     Nothing -> [n]
>   where
>     isFactor f = n `mod` f == 0

Let us try it out

```splice
table "Prime factors of some integers"
    [ col "Number" id # alignRight
    , col "Factors" factors
    , col "Count" $ length . factors
    ] [1..10::Int]
```
