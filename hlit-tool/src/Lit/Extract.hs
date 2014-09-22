module Lit.Extract where

import           Control.Applicative
import           Lit.Walk
import qualified Text.Pandoc.Builder as P

extractCode :: P.Pandoc -> String
extractCode = getConst . walk (const Nothing) f
  where
    f (P.CodeBlock (_, classes, _) str) = Just $
        if "haskell" `elem` classes && "ignore" `notElem` classes
          then Const $ str ++ "\n" else pure undefined
    f _ = Nothing
