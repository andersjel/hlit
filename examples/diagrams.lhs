This document shows how to use *hlit-diagrams-svg* to render one of the
examples from the [diagrams website](http://projects.haskell.org/diagrams/).

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> import Text.Lit.DiagramsSVG (display)

We want to render a Hilbert curve defined by

> hilbert = iterate expand mempty where
>   expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
>              u = vcat [t, alignT $ vrule 1, rotateBy (3/4) t]

Here is what it looks like:

~~~splice
display "A Hilbert curve" 300 $
    pad 1.1 . centerXY $ hilbert!!5
~~~
