module Text.Lit.Table
    ( Table, ColDef
    , table
    , (.//)
    , col, col'
    , bcol, bcol'
    , header, bheader
    , (#)
    , align, width
    , toPandocTable
    , Pandoc.Alignment(..)
    ) where

import           Control.Applicative
import           Data.Foldable       (toList, Foldable)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import           Data.Traversable    (traverse)
import           Text.Lit.Render     (Render (..), RenderBlock (..))
import           Text.Lit.Report     (Report)
import qualified Text.Pandoc.Builder as Pandoc

data Table t = Table [t] (Report Pandoc.Inlines) [ColDef t]
data ColDef t = ColDef
    { _cells     :: t -> Report Pandoc.TableCell
    , _header    :: Maybe (Report Pandoc.TableCell)
    , _width     :: Double
    , _alignment :: Pandoc.Alignment
    }

toPandocTable :: Table t -> Report Pandoc.Block
toPandocTable (Table es caption cs)
    = Pandoc.Table
        <$> (toList <$> caption)
        <*> pure (map _alignment cs)
        <*> pure (map _width cs)
        <*> traverse h cs
        <*> traverse toRow es
  where
    h = fromMaybe (pure []) . _header
    toRow e = traverse (\c -> _cells c $ e) cs

instance RenderBlock (Table t) where
    renderBlock = renderBlock . toPandocTable

def :: ColDef t
def = ColDef undefined Nothing 0 Pandoc.AlignDefault

table :: (Render a, Foldable f) => f t -> a -> Table t
table elements caption = Table (toList elements) (render caption) mempty

infixl 1 .//
(.//) :: Table t -> ColDef t -> Table t
(Table e a cs) .// c = Table e a (cs ++ [c])

col :: (Render a, Render b) => b -> (t -> a) -> ColDef t
col h f = col' f # header h

bcol :: (RenderBlock a, Render b) => b -> (t -> a) -> ColDef t
bcol h f = bcol' f # header h

col' :: Render a => (t -> a) -> ColDef t
col' f = bcol' (\x -> Pandoc.plain <$> render (f x))

bcol' :: RenderBlock a => (t -> a) -> ColDef t
bcol' f = def{_cells=(\e -> toList <$> renderBlock (f e))}

header :: Render a => a -> ColDef t -> ColDef t
header x = bheader $ Pandoc.plain <$> render x

bheader :: RenderBlock a => a -> ColDef t -> ColDef t
bheader x c = c{_header=Just $ toList <$> renderBlock x}

align :: Pandoc.Alignment -> ColDef t -> ColDef t
align a c = c{_alignment=a}

width :: Double -> ColDef t -> ColDef t
width w c = c{_width=w}

infixl 8 #
(#) :: a -> (a -> b) -> b
(#) = flip ($)
