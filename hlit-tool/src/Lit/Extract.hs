module Lit.Extract where

import           Control.Applicative
import qualified Language.Haskell.Exts as H
import           Language.Haskell.Exts.SrcLoc (noLoc)
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
    if (moduleName == "Main") && not (any declIsMain decls)
        then setG (removeMain <$> exports) m
        else m

unqualImport :: H.ModuleName -> H.ImportDecl
#if MIN_VERSION_haskell_src_exts(1,16,0)
unqualImport x = H.ImportDecl noLoc
    x
    False   -- Qualified
    False   -- With SOURCE pragma?
    False   -- Safe import
    Nothing -- Package name
    Nothing -- As ...
    Nothing -- Import specs
#else
unqualImport x = H.ImportDecl noLoc
    x
    False   -- Qualified
    False   -- With SOURCE pragma?
    Nothing -- Package name
    Nothing -- As ...
    Nothing -- Import specs
#endif

extract :: Mode -> P.Pandoc -> Either String H.Module
extract mode doc = let
    ex s = case H.parseModule $ extractBlocks s doc of
        H.ParseOk m -> Right m
        err -> Left $ show err
    impM = case ex code of
        Left err -> Left err
        Right c ->
            modG (unqualImport name:) . modG (++imports) . modG (++pragmas)
                <$> ex includes
          where
            imports :: [H.ImportDecl]
            imports = getG c
            name :: H.ModuleName
            name = getG c
            pragmas :: [H.ModulePragma]
            pragmas = getG c
  in
    case mode of
        Merge    -> fixLackingMain <$> ex codeAndIncludes
        Explicit -> ex includes
        Import   -> fixLackingMain <$> impM
        Auto     -> extract Merge doc
