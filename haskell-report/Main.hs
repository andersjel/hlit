module Main where

import           Control.Monad.IO.Class
import           HaskellReport.RInterp
import           System.Environment     (getArgs)
import           Text.Report.Types      (Report, runReport)

main :: IO ()
main = do
    (infile_:ghcArgs_) <- getArgs
    let options = Options {inputFile=infile_, ghcArgs=ghcArgs_}
    runRInterp options $ do
        report <- evalInline "stuff"
        inlines <- liftIO $ runReport report
        liftIO $ print inlines
