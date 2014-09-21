module Main where

import qualified Lit.Tests.Splice as Splice
import           Test.Framework   (defaultMain)

main :: IO ()
main = defaultMain [Splice.tests]
