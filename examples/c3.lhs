> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE OverloadedStrings  #-}
> import qualified Text.Pandoc.Builder as P
> import qualified Data.Aeson as A
> import Data.Aeson (toJSON, object, (.=), encode)
> import Data.Typeable (Typeable)
> import Text.Lit.Report (ConfigVar, fromTag, modify, get, Report)
> import Data.ByteString.Lazy.Char8 (unpack)
> import Data.HashMap.Strict (insert)
>
> data ChartCountTag = ChartCountTag deriving Typeable
> chartCount :: ConfigVar Int
> chartCount = fromTag ChartCountTag 0
>
> chart :: A.ToJSON a => a -> Report P.Block
> chart ch = do
>   count <- get chartCount
>   modify chartCount (+1)
>   let (A.Object obj) = toJSON ch
>       chartName = "_chart_" ++ show count
>       obj' = insert "bindto" (toJSON $ "#" ++ chartName) obj
>       div = "<div id=\"" ++ chartName ++ "\"></div>"
>       json = unpack $ encode obj'
>       script = "<script>c3.generate(" ++ json ++ ");</script>"
>   return $ P.RawBlock (P.Format "html") $ div ++ script

Lets make a plot:

```splice
  chart $ object
    [ "data" .= object
      [ "columns" .=
        [ "Numbers" : map toJSON [4, 2, 6, 2 :: Int]
        , "Digits" : map toJSON [5, 1, 4, 5 :: Int]
        ]
      ]
    ]
```

---
header-includes:
  <head>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.9/c3.min.css">
    <script charset="utf-8" src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.3/d3.min.js"></script>
    <script charset="utf-8" src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.9/c3.min.js"></script>
  </head>
...
