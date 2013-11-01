module Lit.Extract
    ( extract
    , Extraction (..)
    ) where

import           Control.Applicative
import qualified Data.Aeson            as Aeson
import           Data.Char             (isSpace)
import           Data.Monoid
import           Data.Traversable
import qualified Language.Haskell.Exts as H
import           Lit.Splice            (Splice)
import qualified Lit.Splice            as Splice
import           Text.Pandoc.Builder   (Block (..), Inline (..), Pandoc (..))
import qualified Text.Pandoc.Builder   as P

data Extraction a = Extraction
    String     -- ^ Extracted code
    (Splice a) -- ^ Extracted document

instance Functor Extraction where
    fmap f (Extraction code spl) = Extraction code $ fmap f spl

instance Applicative Extraction where
    pure x = Extraction "" $ pure x
    (Extraction codeX x) <*> (Extraction codeY y)
        = Extraction (codeX ++ codeY) $ x <*> y

xSplice :: Aeson.FromJSON a => H.Type -> H.Exp -> Extraction a
xSplice t e = Extraction "" $ Splice.splice t e

xCode :: String -> Extraction ()
xCode s = Extraction s $ pure ()

extract :: Pandoc -> Extraction Pandoc
extract (Pandoc meta blocks) = Pandoc meta <$> exBlocks blocks

exElements :: (a -> Extraction [a]) -> [a] -> Extraction [a]
exElements f es = concat <$> traverse f es

exBlocks :: [Block] -> Extraction [Block]
exBlocks = exElements exBlock

exInlines :: [Inline] -> Extraction [Inline]
exInlines = exElements exInline

singleton :: a -> [a]
singleton = (:[])

hasInlines :: ([Inline] -> a) -> [Inline] -> Extraction [a]
hasInlines f inlines = singleton . f <$> exInlines inlines

exBlock :: Block -> Extraction [Block]
exBlock (Plain inlines) = hasInlines Plain inlines
exBlock (Para inlines) = hasInlines Para inlines
exBlock (CodeBlock attr@(_, classes, _) str) = case () of
    _ | "haskell" `elem` classes -> xCode (str ++ "\n") *> b
    _ | "splice" `elem` classes  -> xSplice blockType $ blockExpr str
    _ | "do" `elem` classes      -> xSplice blockType $ blockExpr $ "do\n" ++ str
    _                            -> b
  where
    b = pure $ if "hidden" `elem` classes
        then mempty
        else [CodeBlock attr str]
exBlock r@(RawBlock _ _) = pure [r]
exBlock (BlockQuote blocks) = pure . BlockQuote <$> exBlocks blocks
exBlock (OrderedList attrs content) =
    singleton . OrderedList attrs <$> traverse exBlocks content
exBlock (BulletList content) =
    singleton . BulletList <$> traverse exBlocks content
exBlock (DefinitionList content) =
    singleton . DefinitionList <$> traverse item content
  where
    item (term, definitions) =
        (,) <$> exInlines term <*> traverse exBlocks definitions
exBlock (Header n attr inlines) = hasInlines (Header n attr) inlines
exBlock r@HorizontalRule = pure [r]
exBlock (Table caption alignment widths headers content) = singleton <$> table
  where
    table = Table
        <$> exInlines caption
        <*> pure alignment
        <*> pure widths
        <*> traverse exBlocks headers
        <*> traverse (traverse exBlocks) content
exBlock (Div attr blocks) = singleton . Div attr <$> exBlocks blocks
exBlock r@Null = pure [r]

exInline :: Inline -> Extraction [Inline]
exInline r@(Str _) = pure [r]
exInline (Emph inlines) = hasInlines Emph inlines
exInline (Strong inlines) = hasInlines Strong inlines
exInline (Strikeout inlines) = hasInlines Strikeout inlines
exInline (Superscript inlines) = hasInlines Superscript inlines
exInline (Subscript inlines) = hasInlines Subscript inlines
exInline (SmallCaps inlines) = hasInlines SmallCaps inlines
exInline (Quoted typ inlines) = hasInlines (Quoted typ) inlines
exInline (Cite cs inlines) = singleton <$> cite
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
        '@' : expr -> xSplice inlineType  $ inlineExpr expr
        _ -> pure [Code attr str]
exInline r@Space = pure [r]
exInline r@LineBreak = pure [r]
exInline r@(Math _ _) = pure [r]
exInline r@(RawInline _ _) = pure [r]
exInline (Link inlines targ) = hasInlines (flip Link targ) inlines
exInline (Image inlines targ) = hasInlines (flip Image targ) inlines
exInline (Note blocks) = singleton . Note <$> exBlocks blocks
exInline (Span attr inlines) = hasInlines (Span attr) inlines

blockType :: H.Type
blockType = typ
  where
    H.ParseOk typ = H.parse "Text.Lit.Report.Report [Text.Pandoc.Builder.Block]"

renderExpr :: String -> String -> H.Exp
renderExpr func s = case H.parse s of
    H.ParseOk x -> H.app f x
    failure     -> H.app (H.function "fail") (H.strE $ show failure)
  where 
    H.ParseOk f = H.parse $
        "fmap Data.Foldable.toList . Text.Lit.Render." ++ func 

blockExpr :: String -> H.Exp
blockExpr = renderExpr "renderBlock"

inlineType :: H.Type
inlineType = typ
  where
    H.ParseOk typ = H.parse "Text.Lit.Report.Report [Text.Pandoc.Builder.Inline]"

inlineExpr :: String -> H.Exp
inlineExpr = renderExpr "render"
