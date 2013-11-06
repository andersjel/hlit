module Main where

import           Test.Framework       (defaultMain)
import qualified Text.Lit.RenderFloat as RenderFloat

main :: IO ()
main = defaultMain [RenderFloat.tests]
