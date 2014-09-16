module Text.Lit.DiagramsSVG (renderSvg) where

import qualified Text.Lit.Report              as R
import qualified Text.Pandoc.Builder          as P
import qualified Text.Blaze.Svg.Renderer.Utf8 as B
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as S
import           Text.Lit.Render              (Render, render)
import           Data.Foldable                (toList)
import           Control.Applicative

renderSvg
    :: Render a
    => String                -- ^ title
    -> a                     -- ^ alt text
    -> D.SizeSpec2D
    -> D.Diagram S.SVG D.R2
    -> R.Report P.Inline     -- ^ inline image
renderSvg title alt size diagram = do
    let im = D.renderDia S.SVG (S.SVGOptions size Nothing) diagram
    url <- R.saveOutputFile "diagram" "svg" $ B.renderSvg im
    alt' <- toList <$> render alt
    return $ P.Image alt' (url, title)
