module Lit.Extract
    ( extract
    , Extract(..)
    ) where

import           Control.Applicative
import           Data.Traversable
import           Text.Pandoc.Builder (Block (..), Inline (..), Pandoc (..))
import qualified Text.Pandoc.Builder as P

data Extract f = Extract
    { inlineCode :: P.Inline -> f [P.Inline]
    , blockCode  :: P.Block -> f [P.Block]
    }

extract :: Applicative f => Extract f -> Pandoc -> f Pandoc
extract e (Pandoc meta blocks) = Pandoc meta <$> exBlocks e blocks

exElements :: Applicative f => (a -> f [a]) -> [a] -> f [a]
exElements f es = concat <$> traverse f es

exBlocks :: Applicative f => Extract f -> [Block] -> f [Block]
exBlocks e = exElements (exBlock e)

exInlines :: Applicative f => Extract f -> [Inline] -> f [Inline]
exInlines e = exElements (exInline e)

singleton :: a -> [a]
singleton = (:[])

hasInlines :: Applicative f => Extract f -> ([Inline] -> a) -> [Inline] -> f [a]
hasInlines e f inlines = singleton . f <$> exInlines e inlines

exBlock :: Applicative f => Extract f -> Block -> f [Block]
exBlock e (Plain inlines) = hasInlines e Plain inlines
exBlock e (Para inlines) = hasInlines e Para inlines
exBlock e r@(CodeBlock _ _) = blockCode e r
exBlock e (BlockQuote blocks) = pure . BlockQuote <$> exBlocks e blocks
exBlock e (OrderedList attrs content) =
    singleton . OrderedList attrs <$> traverse (exBlocks e) content
exBlock e (BulletList content) =
    singleton . BulletList <$> traverse (exBlocks e) content
exBlock e (DefinitionList content) =
    singleton . DefinitionList <$> traverse item content
  where
    item (term, definitions) =
        (,) <$> exInlines e term <*> traverse (exBlocks e) definitions
exBlock e (Header n attr inlines) = hasInlines e (Header n attr) inlines
exBlock e (Table caption alignment widths headers content) = singleton <$> table
  where
    table = Table
        <$> exInlines e caption
        <*> pure alignment
        <*> pure widths
        <*> traverse (exBlocks e) headers
        <*> traverse (traverse (exBlocks e)) content
exBlock e (Div attr blocks) = singleton . Div attr <$> exBlocks e blocks
exBlock _ r = pure [r]

exInline :: Applicative f => Extract f -> Inline -> f [Inline]
exInline e (Emph inlines) = hasInlines e Emph inlines
exInline e (Strong inlines) = hasInlines e Strong inlines
exInline e (Strikeout inlines) = hasInlines e Strikeout inlines
exInline e (Superscript inlines) = hasInlines e Superscript inlines
exInline e (Subscript inlines) = hasInlines e Subscript inlines
exInline e (SmallCaps inlines) = hasInlines e SmallCaps inlines
exInline e (Quoted typ inlines) = hasInlines e (Quoted typ) inlines
exInline e (Cite cs inlines) = singleton <$> cite
  where
    cite = Cite <$> traverse f cs <*> exInlines e inlines
    f c = P.Citation
        <$> pure (P.citationId c)
        <*> exInlines e (P.citationPrefix c)
        <*> exInlines e (P.citationSuffix c)
        <*> pure (P.citationMode c)
        <*> pure (P.citationNoteNum c)
        <*> pure (P.citationHash c)
exInline e r@(Code _ _) = inlineCode e r
exInline e (Link inlines targ) = hasInlines e (flip Link targ) inlines
exInline e (Image inlines targ) = hasInlines e (flip Image targ) inlines
exInline e (Note blocks) = singleton . Note <$> exBlocks e blocks
exInline e (Span attr inlines) = hasInlines e (Span attr) inlines
exInline _ r = pure [r]
