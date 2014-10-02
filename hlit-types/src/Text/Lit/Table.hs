module Text.Lit.Table
    ( Table, Column
    , table
    , col, blockCol
    , inline
    , (#)
    , alignLeft, alignRight, alignCenter
    , width
    ) where

import           Control.Applicative
import           Data.Foldable       (toList, Foldable)
import           Data.Traversable    (traverse)
import           Text.Lit.Render     (Render (..), RenderBlock (..))
import           Text.Lit.Report     (Report)
import qualified Text.Pandoc.Builder as Pandoc

type Table = Report Pandoc.Block
data Column t = Column
    { _format    :: t -> Report Pandoc.TableCell
    , _header    :: Report Pandoc.TableCell
    , _width     :: Double
    , _alignment :: Pandoc.Alignment
    }

table :: (Render a, Foldable f) => a -> [Column t] -> f t -> Table
table caption columns records
    = Pandoc.Table
        <$> (toList <$> render caption)
        <*> pure (map _alignment columns)
        <*> pure (map _width columns)
        <*> traverse _header columns
        <*> traverse toRow (toList records)
  where
    toRow record = traverse (\c -> _format c $ record) columns

inline :: Render a => a -> Report Pandoc.Blocks
inline x = Pandoc.plain <$> render x

col :: (Render a, Render b) => b -> (t -> a) -> Column t
col header f = blockCol (inline header) (inline . f)

blockCol :: (RenderBlock a, RenderBlock b) => b -> (t -> a) -> Column t
blockCol header f = Column
    { _format=(\e -> toList <$> renderBlock (f e))
    , _header=toList <$> renderBlock header
    , _width=0, _alignment=Pandoc.AlignDefault
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
