module Lit.Tests.Splice where

import           Control.Applicative
import           Data.Default
import qualified Language.Haskell.Exts          as H
import           Lit.Splice
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test)

tests :: Test
tests = testGroup "Splice"
    [ testCase "basic_splice" case_basic_splice
    ]

case_basic_splice :: Assertion
case_basic_splice = do
    let p s = x
          where H.ParseOk x = H.parse s
        mkSpl t x = splice (p t) (p x)
        testSplice = (,,) <$> mkSpl "IO Int" "return 10" 
                          <*> pure ()
                          <*> mkSpl "IO String" "return \"test\""
    r <- runSplice def () testSplice
    v <- case r of
        Left x -> error $ show x
        Right x -> return x
    v @?= (10 :: Int, (), "test")
