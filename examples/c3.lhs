> {-# LANGUAGE OverloadedStrings  #-}
> import Data.Aeson (toJSON, object, (.=))
> import Text.Lit.C3js (chart)

Lets make some plots:

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

```splice
  chart $ object
    [ "data" .= object
      [ "columns" .=
        [ "Numbers" : map toJSON [2, 9, 1, 3 :: Int]
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
