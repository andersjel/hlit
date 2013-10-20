module Main where

import           Test.Framework       (defaultMain)
import qualified Text.Lit.RenderFloat as RenderFloat

main = defaultMain [RenderFloat.tests]
