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
    H.ModuleName name = getG m
    isMainModule = name == "Main"
    decls :: [H.Decl]
    decls = getG m
    exports :: Maybe [H.ExportSpec]
    exports = getG m
    exports' = removeMain <$> exports
    removeMain :: [H.ExportSpec] -> [H.ExportSpec]
    removeMain = filter (not . specIsMain)
    m' = setG exports' m
#if MIN_VERSION_haskell_src_exts(1,16,0)
    specIsMain (H.EVar _ (H.UnQual (H.Ident "main"))) = True
#else
    specIsMain (H.EVar (H.UnQual (H.Ident "main"))) = True
#endif
    specIsMain _ = False
    hasMain = any isMain decls
    isMain :: H.Decl -> Bool
#if MIN_VERSION_haskell_src_exts(1,16,0)
    isMain (H.PatBind _ (H.PVar (H.Ident "main")) _ _ ) = True
#else
    isMain (H.PatBind _ (H.PVar (H.Ident "main")) _ _ _) = True
#endif
    isMain _ = False
  in
    if isMainModule && not hasMain then m' else m

extract :: Mode -> P.Pandoc -> Either String H.Module
extract _ d
    = case H.parseModule $ extractBlocks codeAndIncludes d of
        H.ParseOk m -> Right $ fixLackingMain m
        err -> Left $ show err
