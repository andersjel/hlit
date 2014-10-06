{-# LANGUAGE DeriveDataTypeable #-}

module Text.Lit.ChartDiagrams
    ( inlinePlot
    , plot
    , ecInlinePlot
    , ecPlot
    , options
    , size
    , format
    , customFonts
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class                    (liftIO)
import           Data.Default
import           Data.Foldable                             (toList)
import           Data.Map                                  (Map)
import           Data.Typeable                             (Typeable)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as C
import qualified Graphics.Rendering.Chart.Backend.Types    as C
import qualified Graphics.Rendering.Chart.Renderable       as C
import qualified Graphics.Rendering.Chart.State            as C
import           Text.Lit.Render                           (Render, render)
import qualified Text.Lit.Report                           as R
import qualified Text.Pandoc.Builder                       as P

data Options = Options {unOptions :: C.FileOptions} deriving Typeable

instance Default Options where
    def = Options def

options_ :: R.ConfigVar Options
options_ = R.singleton

options :: R.ConfigVar C.FileOptions
options = R.refine options_ unOptions (const . Options)

size :: R.ConfigVar (Double, Double)
size = R.refine options C._fo_size $ \x y -> y{C._fo_size=x}

format :: R.ConfigVar (C.FileFormat)
format = R.refine options C._fo_format $ \x y -> y{C._fo_format=x}

customFonts :: R.ConfigVar (Map (String, C.FontSlant, C.FontWeight) FilePath)
customFonts = R.refine options C._fo_customFonts $ \x y -> y{C._fo_customFonts=x}

-- | This is the most general way to render a plot with this module. Note,
-- that this function returns an inline.
inlinePlot
    :: Render a
    => String             -- ^ title
    -> a                  -- ^ alt text
    -> (C.Renderable r)   -- ^ plot to render
    -> R.Report P.Inline  -- ^ inline image
inlinePlot title alt p = do
    op <- R.get options
    let ex = case C._fo_format op of
            C.EPS -> "eps"
            C.SVG -> "svg"
            C.SVG_EMBEDDED -> "svg"
    (path, url) <- R.reserveOutputPath "plot" ex
    liftIO $ C.renderableToFile op path p
    alt' <- toList <$> render alt
    return $ P.Image alt' (url, title)

-- | Put a plot in a paragraph by itself.
--
-- If the `implicit_figures` pandoc extension is in use, then the paragraph is
-- rendered as a figure with the alt-text as the caption.
plot
    :: Render a
    => a                 -- ^ alt text
    -> C.Renderable r    -- ^ plot to render
    -> R.Report P.Block  -- ^ a paragraph with the diagram in it
plot alt p
    = P.Para . (:[]) <$> inlinePlot "" alt p

ecInlinePlot
    :: (Render a, C.ToRenderable r, Default r)
    => String             -- ^ title
    -> a                  -- ^ alt text
    -> C.EC r ()          -- ^ plot to render
    -> R.Report P.Inline  -- ^ inline image
ecInlinePlot title alt = inlinePlot title alt . C.toRenderable . C.execEC

ecPlot
    :: (Render a, C.ToRenderable r, Default r)
    => a                 -- ^ alt text
    -> C.EC r ()         -- ^ plot to render
    -> R.Report P.Block  -- ^ a paragraph with the diagram in it
ecPlot alt = plot alt . C.toRenderable . C.execEC
