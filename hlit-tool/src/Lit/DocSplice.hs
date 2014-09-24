module Lit.DocSplice
    ( mk
    , options
    ) where

import           Control.Applicative
import           Data.Char                    (isSpace)
import           Data.Default
import qualified Language.Haskell.Exts        as H
import           Language.Haskell.Exts.SrcLoc
import           Lit.Generic
import           Lit.Splice                   (Splice, splice)
import qualified Lit.Splice                   as Splice
import qualified Text.Pandoc.Builder          as P

sureParse :: H.Parseable a => String -> a
sureParse s = let H.ParseOk t = H.parse s in t

blockType, inlineType :: H.Type
blockType = sureParse "Text.Lit.Internal.BlockType"
inlineType = sureParse "Text.Lit.Internal.InlineType"

renderExpr :: String -> String -> H.Exp
renderExpr func s = case H.parse s of
    H.ParseOk x -> H.app f x
    failure     -> H.app (H.function "fail") (H.strE $ show failure)
  where
    f = sureParse $ "Text.Lit.Internal." ++ func

blockExpr, inlineExpr :: String -> H.Exp
blockExpr  = renderExpr "renderBlock_"
inlineExpr = renderExpr "render_"

block :: P.Block -> Maybe (Splice [P.Block])
block r@(P.CodeBlock (_, classes, _) str) = Just $
    case () of
        _ | "splice" `elem` classes -> f str
        _ | "do"     `elem` classes -> f $ "do\n" ++ str
        _ -> pure $ if "hidden" `elem` classes then [] else [r]
  where f s = splice blockType $ blockExpr s
block _ = Nothing

inline :: P.Inline -> Maybe (Splice [P.Inline])
inline r@(P.Code attr str) = Just $
    case dropWhile isSpace str of
        '@' : '@' : rest -> pure [P.Code attr $ '@' : rest]
        '@' : expr -> splice inlineType $ inlineExpr expr
        _ -> pure [r]
inline _ = Nothing

mk :: P.Pandoc -> Splice P.Pandoc
mk = walk inline block

qualifiedImport :: String -> H.ImportDecl
#if MIN_VERSION_haskell_src_exts(1,16,0)
qualifiedImport x = H.ImportDecl noLoc
    (H.ModuleName x)
    True    -- Qualified
    False   -- With SOURCE pragma?
    False   -- Safe import
    Nothing -- Package name
    Nothing -- As ...
    Nothing -- Import specs
#else
qualifiedImport x = H.ImportDecl noLoc
    (H.ModuleName x)
    True    -- Qualified
    False   -- With SOURCE pragma?
    Nothing -- Package name
    Nothing -- As ...
    Nothing -- Import specs
#endif

options :: Splice.Options
options = def
    { Splice.spliceImports = [qualifiedImport "Text.Lit.Internal"]
    , Splice.mainImports   = [qualifiedImport "Text.Lit.Report"]
    , Splice.mainRun       = sureParse "Text.Lit.Report.runReport"
    , Splice.outputDir     = Nothing
    }
