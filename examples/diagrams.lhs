Import the diagrams module and the rendering functions

> import Text.Lit.DiagramsSVG
> import Diagrams.Prelude

Here is a circle:

`@renderSvg "" "A circle" (Width 100) $ pad 1.1 $ circle 1`
