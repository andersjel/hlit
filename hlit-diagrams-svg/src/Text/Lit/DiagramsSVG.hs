module Text.Lit.DiagramsSVG (diagram, display) where

import           Control.Applicative
import           Data.Foldable                (toList)
import qualified Diagrams.Backend.SVG         as S
import qualified Diagrams.Prelude             as D
import qualified Text.Blaze.Svg.Renderer.Utf8 as B
import           Text.Lit.Render              (Render, render)
import qualified Text.Lit.Report              as R
import qualified Text.Pandoc.Builder          as P

-- | This is the most general way to render a diagram with this module. Note,
-- that this function returns an inline.
diagram
    :: Render a
    => String                -- ^ title
    -> a                     -- ^ alt text
    -> D.SizeSpec2D
    -> D.Diagram S.SVG D.R2
    -> R.Report P.Inline     -- ^ inline image
diagram title alt size dia = do
    let im = D.renderDia S.SVG (S.SVGOptions size Nothing) dia
    url <- R.saveOutputFile "diagram" "svg" $ B.renderSvg im
    alt' <- toList <$> render alt
    return $ P.Image alt' (url, title)

-- | Put a diagram in a paragraph by itself.
--
-- If the `implicit_figures` pandoc extension is in use, then the paragraph is
-- rendered as a figure with the alt-text as the caption.
display
    :: Render a
    => a                     -- ^ alt text
    -> Double                -- ^ width (pixels)
    -> D.Diagram S.SVG D.R2
    -> R.Report P.Block      -- ^ a paragraph with the diagram in it
display alt width dia
    = P.Para . (:[]) <$> diagram "" alt (D.Width width) dia
