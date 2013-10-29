module Main where

import qualified Lit.Splice           as Splice
import           Test.Framework       (defaultMain)
import qualified Text.Lit.RenderFloat as RenderFloat

main :: IO ()
main = defaultMain [RenderFloat.tests, Splice.tests]
