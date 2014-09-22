module Lit.Extract where

import           Control.Applicative
import           Lit.Walk
import qualified Text.Pandoc.Builder as P

code :: [String] -> Bool
code cs = "haskell" `elem` cs && "ignore" `notElem` cs

includes :: [String] -> Bool
includes cs = "include" `elem` cs

codeAndIncludes :: [String] -> Bool
codeAndIncludes cs = code cs || includes cs

extract :: ([String] -> Bool) -> P.Pandoc -> String
extract selector = getConst . walk (const Nothing) f
  where
    f (P.CodeBlock (_, classes, _) str) = Just $
        if selector classes
          then Const $ str ++ "\n" else pure undefined
    f _ = Nothing
