module Lit.MkDoc
    ( extractSplice
    , extractCode
    , docSpliceOptions
    ) where

import           Control.Applicative
import           Data.Char                    (isSpace)
import           Data.Default
import qualified Language.Haskell.Exts        as H
import           Language.Haskell.Exts.SrcLoc
import           Lit.Walk
import           Lit.Splice                   (Splice, splice)
import qualified Lit.Splice                   as Splice
import qualified Text.Pandoc.Builder          as P

sureParse :: H.Parseable a => String -> a
sureParse s = let H.ParseOk t = H.parse s in t

blockType, inlineType :: H.Type
blockType = sureParse
    "Text.Lit.Report.Report [Text.Pandoc.Builder.Block]"
inlineType = sureParse
    "Text.Lit.Report.Report [Text.Pandoc.Builder.Inline]"

renderExpr :: String -> String -> H.Exp
renderExpr func s = case H.parse s of
    H.ParseOk x -> H.app f x
    failure     -> H.app (H.function "fail") (H.strE $ show failure)
  where
    f = sureParse $ "fmap Data.Foldable.toList . Text.Lit.Render." ++ func

blockExpr, inlineExpr :: String -> H.Exp
blockExpr  = renderExpr "renderBlock"
inlineExpr = renderExpr "render"

spliceBlock :: P.Block -> Maybe (Splice [P.Block])
spliceBlock r@(P.CodeBlock (_, classes, _) str) = Just $ 
    case () of
        _ | "splice" `elem` classes -> f str
        _ | "do"     `elem` classes -> f $ "do\n" ++ str
        _ -> pure $ if "hidden" `elem` classes then [] else [r]
  where f s = splice blockType $ blockExpr s
spliceBlock _ = Nothing

spliceInline :: P.Inline -> Maybe (Splice [P.Inline])
spliceInline r@(P.Code _ str) = Just $
    case dropWhile isSpace str of
        '@' : expr -> splice inlineType $ inlineExpr expr
        _ -> pure [r]
spliceInline _ = Nothing

extractSplice :: P.Pandoc -> Splice P.Pandoc
extractSplice = walk spliceInline spliceBlock

extractCode :: P.Pandoc -> String
extractCode = getConst . walk (const Nothing) f
  where 
    f (P.CodeBlock (_, classes, _) str) = Just $
        if "haskell" `elem` classes && "ignore" `notElem` classes
          then Const $ str ++ "\n" else pure []
    f _ = Nothing

qualifiedImport :: String -> H.ImportDecl
qualifiedImport x = H.ImportDecl noLoc
    (H.ModuleName x)
    True    -- Qualified
    False   -- With SOURCE pragma?
    Nothing -- Package name
    Nothing -- As ...
    Nothing -- Import specs

docSpliceOptions :: Splice.Options
docSpliceOptions = def
    { Splice.spliceImports =
        [ qualifiedImport "Text.Lit.Render"
        , qualifiedImport "Data.Foldable"
        , qualifiedImport "Text.Pandoc.Builder"
        ]
    , Splice.mainImports   = [qualifiedImport "Text.Lit.Report"]
    , Splice.mainRun       =
        sureParse "Text.Lit.Report.runReport"
    , Splice.outputDir     = Nothing
    }
