module Lit.Extract where

import           Control.Applicative
import qualified Data.Data             as Data
import qualified Data.Typeable         as Typeable
import qualified Language.Haskell.Exts as H
import           Lit.Walk
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

getG :: (Data.Data a, Typeable.Typeable b) => a -> b
getG a = r
  where
    Left r = Data.gmapM f a
    f :: (Typeable.Typeable b, Data.Data d) => d -> Either b d
    f x = maybe (Right x) Left $ Typeable.cast x

fixLackingMain :: H.Module -> H.Module
fixLackingMain m = let
    H.ModuleName name = getG m
    isMain = name == "Main"
    decls :: [H.Decl]
    decls = getG m
    -- TODO: scan decls for a declaration of main. If it does not exist, strip
    -- it from the exports.
  in
    if not isMain then m else
        undefined

-- TODO:
-- extract :: Mode -> P.Pandoc -> Either String H.Module

-- for now
extract :: ([String] -> Bool) -> P.Pandoc -> String
extract = extractBlocks
