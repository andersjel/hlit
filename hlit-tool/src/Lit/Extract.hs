module Lit.Extract where

import           Control.Applicative
import qualified Language.Haskell.Exts as H
import           Lit.Generic
import qualified Text.Pandoc.Builder   as P

data Mode = Auto | Merge | Import | Explicit

code :: [String] -> Bool
code cs = "haskell" `elem` cs && "ignore" `notElem` cs

includes :: [String] -> Bool
includes cs = "include" `elem` cs

codeAndIncludes :: [String] -> Bool
codeAndIncludes cs = code cs || includes cs

extractBlocks :: ([String] -> Bool) -> P.Pandoc -> String
extractBlocks selector = getConst . walk (const Nothing) f
  where
    f (P.CodeBlock (_, classes, _) str) = Just $
        if selector classes
          then Const $ str ++ "\n" else pure undefined
    f _ = Nothing

fixLackingMain :: H.Module -> H.Module
fixLackingMain m = let

    H.ModuleName moduleName = getG m

    decls :: [H.Decl]
    decls = getG m

    exports :: Maybe [H.ExportSpec]
    exports = getG m

    removeMain :: [H.ExportSpec] -> [H.ExportSpec]
    removeMain = filter (not . exportIsMain)

    exportIsMain :: H.ExportSpec -> Bool
#if MIN_VERSION_haskell_src_exts(1,16,0)
    exportIsMain (H.EVar _ (H.UnQual (H.Ident "main"))) = True
#else
    exportIsMain (H.EVar (H.UnQual (H.Ident "main"))) = True
#endif
    exportIsMain _ = False

    declIsMain :: H.Decl -> Bool
#if MIN_VERSION_haskell_src_exts(1,16,0)
    declIsMain (H.PatBind _ (H.PVar (H.Ident "main")) _ _ ) = True
#else
    declIsMain (H.PatBind _ (H.PVar (H.Ident "main")) _ _ _) = True
#endif
    declIsMain _ = False

  in
    if (moduleName == "main") && not (any declIsMain decls)
        then setG (removeMain <$> exports) m
        else m

extract :: Mode -> P.Pandoc -> Either String H.Module
extract _ d
    = case H.parseModule $ extractBlocks codeAndIncludes d of
        H.ParseOk m -> Right $ fixLackingMain m
        err -> Left $ show err
