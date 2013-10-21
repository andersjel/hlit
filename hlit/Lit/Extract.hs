module Lit.Extract
    ( extract
    ) where

import           Control.Applicative
import           Data.Char           (isSpace)
import           Data.Foldable       (toList)
import           Data.Traversable
import           Lit.Interp
import           Text.Lit.Report
import           Text.Pandoc.Builder (Block (..), Inline (..), Pandoc (..))
import qualified Text.Pandoc.Builder as P

newtype IR a = IR { unIR :: Interp (Report a) }

instance Functor IR where
    fmap f (IR x) = IR $ fmap (fmap f) x

instance Applicative IR where
    pure x = IR $ pure $ pure x
    (IR f) <*> (IR x) = IR $ (<*>) <$> f <*> x

extract :: Pandoc -> Interp (Report Pandoc)
extract (Pandoc meta blocks) = unIR $ Pandoc meta <$> exBlocks blocks

exElements :: (a -> IR [a]) -> [a] -> IR [a]
exElements f es = concat <$> traverse f es

exBlocks :: [Block] -> IR [Block]
exBlocks = exElements exBlock

exInlines :: [Inline] -> IR [Inline]
exInlines = exElements exInline

hasInlines :: ([Inline] -> a) -> [Inline] -> IR [a]
hasInlines f inlines = pure . f <$> exInlines inlines

exBlock :: Block -> IR [Block]
exBlock (Plain inlines) = hasInlines Plain inlines
exBlock (Para inlines) = hasInlines Para inlines
exBlock (CodeBlock attr str) =
    case dropWhile isSpace str of
        '@' : '\\' : expr -> IR (exec expr) *> pure []
        '@' : expr -> toList <$> IR (evalBlock expr)
        _ -> pure $ pure $ CodeBlock attr str
exBlock r@(RawBlock _ _) = pure $ pure r
exBlock (BlockQuote blocks) = pure . BlockQuote <$> exBlocks blocks
exBlock (OrderedList attrs content) =
    pure . OrderedList attrs <$> traverse exBlocks content
exBlock (BulletList content) =
    pure . BulletList <$> traverse exBlocks content
exBlock (DefinitionList content) =
    pure . DefinitionList <$> traverse item content
  where
    item (term, definitions) =
        (,) <$> exInlines term <*> traverse exBlocks definitions
exBlock (Header n attr inlines) = hasInlines (Header n attr) inlines
exBlock r@HorizontalRule = pure $ pure r
exBlock (Table caption alignment widths headers content) = pure <$> table
  where
    table = Table
        <$> exInlines caption
        <*> pure alignment
        <*> pure widths
        <*> traverse exBlocks headers
        <*> traverse (traverse exBlocks) content
exBlock (Div attr blocks) = pure . Div attr <$> exBlocks blocks
exBlock r@Null = pure $ pure r

exInline :: Inline -> IR [Inline]
exInline r@(Str _) = pure $ pure r
exInline (Emph inlines) = hasInlines Emph inlines
exInline (Strong inlines) = hasInlines Strong inlines
exInline (Strikeout inlines) = hasInlines Strikeout inlines
exInline (Superscript inlines) = hasInlines Superscript inlines
exInline (Subscript inlines) = hasInlines Subscript inlines
exInline (SmallCaps inlines) = hasInlines SmallCaps inlines
exInline (Quoted typ inlines) = hasInlines (Quoted typ) inlines
exInline (Cite cs inlines) = pure <$> cite
  where
    cite = Cite <$> traverse f cs <*> exInlines inlines
    f c = P.Citation
        <$> pure (P.citationId c)
        <*> exInlines (P.citationPrefix c)
        <*> exInlines (P.citationSuffix c)
        <*> pure (P.citationMode c)
        <*> pure (P.citationNoteNum c)
        <*> pure (P.citationHash c)
exInline (Code attr str) =
    case dropWhile isSpace str of
        '@' : expr -> toList <$> IR (evalInline expr)
        _ -> pure $ pure $ Code attr str
exInline r@Space = pure $ pure r
exInline r@LineBreak = pure $ pure r
exInline r@(Math _ _) = pure $ pure r
exInline r@(RawInline _ _) = pure $ pure r
exInline (Link inlines targ) = hasInlines (flip Link targ) inlines
exInline (Image inlines targ) = hasInlines (flip Image targ) inlines
exInline (Note blocks) = pure . Note <$> exBlocks blocks
exInline (Span attr inlines) = hasInlines (Span attr) inlines
