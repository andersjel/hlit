module HaskellReport.Extract
    ( extract
    ) where

import           Control.Applicative
import           Data.Char            (isSpace)
import           Data.Foldable        (toList)
import           Data.Traversable
import           HaskellReport.Interp
import           Text.Pandoc.Builder  (Block (..), Inline (..), Pandoc (..))
import           Text.Report.Types

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
exBlock (Plain inlines) =
    hasInlines Plain inlines
exBlock (Para inlines) =
    hasInlines Para inlines
exBlock (CodeBlock attr str) =
    case dropWhile isSpace str of
        '@' : expr -> toList <$> IR (evalBlock expr)
        _ -> pure $ pure $ CodeBlock attr str
exBlock r@(RawBlock _ _) =
    pure $ pure r
exBlock (BlockQuote blocks) =
    pure . BlockQuote <$> exBlocks blocks
exBlock (OrderedList attrs content) =
    pure . OrderedList attrs <$> traverse exBlocks content
exBlock (BulletList content) =
    pure . BulletList <$> traverse exBlocks content
exBlock (DefinitionList content) =
    pure . DefinitionList <$> traverse item content
  where
    item (term, definitions) =
        (,) <$> exInlines term <*> traverse exBlocks definitions
exBlock (Header n attr inlines) =
    hasInlines (Header n attr) inlines
exBlock r@HorizontalRule =
    pure $ pure r
exBlock (Table caption alignment widths headers content) =
    pure <$> table
  where
    table = Table
        <$> exInlines caption
        <*> pure alignment
        <*> pure widths
        <*> traverse exBlocks headers
        <*> traverse (traverse exBlocks) content
exBlock (Div attr blocks) =
    pure . Div attr <$> exBlocks blocks
exBlock r@Null =
    pure $ pure r

exInline :: Inline -> IR [Inline]
exInline = undefined
