module Text.Lit.Table
    ( Table, Column
    , table
    , col, blockCol
    , inline
    , (#)
    , alignLeft, alignRight, alignCenter
    , width
    , toPandocTable
    ) where

import           Control.Applicative
import           Data.Foldable       (toList, Foldable)
import           Data.Monoid
import           Data.Traversable    (traverse)
import           Text.Lit.Render     (Render (..), RenderBlock (..))
import           Text.Lit.Report     (Report)
import qualified Text.Pandoc.Builder as Pandoc

data Table t = Table [t] (Report Pandoc.Inlines) [Column t]
data Column t = Column
    { _cells     :: t -> Report Pandoc.TableCell
    , _header    :: Report Pandoc.TableCell
    , _width     :: Double
    , _alignment :: Pandoc.Alignment
    }

toPandocTable :: Table t -> Report Pandoc.Block
toPandocTable (Table es caption cs)
    = Pandoc.Table
        <$> (toList <$> caption)
        <*> pure (map _alignment cs)
        <*> pure (map _width cs)
        <*> traverse _header cs
        <*> traverse toRow es
  where
    toRow e = traverse (\c -> _cells c $ e) cs

instance RenderBlock (Table t) where
    renderBlock = renderBlock . toPandocTable

def :: Column t
def = Column undefined undefined 0 Pandoc.AlignDefault

table :: (Render a, Foldable f) => a -> [Column t] -> f t -> Table t
table caption columns elements
    = Table (toList elements) (render caption) columns

inline :: Render a => a -> Report Pandoc.Blocks
inline x = Pandoc.plain <$> render x

col :: (Render a, Render b) => b -> (t -> a) -> Column t
col h f = blockCol (inline h) (inline . f)

blockCol :: (RenderBlock a, RenderBlock b) => b -> (t -> a) -> Column t
blockCol h f = def
    { _cells=(\e -> toList <$> renderBlock (f e))
    , _header=toList <$> renderBlock h
    }

alignLeft, alignRight, alignCenter :: Column t -> Column t
alignLeft c = c{_alignment=Pandoc.AlignLeft}
alignRight c = c{_alignment=Pandoc.AlignRight}
alignCenter c = c{_alignment=Pandoc.AlignCenter}

width :: Double -> Column t -> Column t
width w c = c{_width=w}

infixl 8 #
(#) :: a -> (a -> b) -> b
(#) = flip ($)
