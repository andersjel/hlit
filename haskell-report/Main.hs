module Main where

import           Control.Applicative
import           Control.Monad          (unless)
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified HaskellReport.Interp   as Interp
import           System.Console.GetOpt  (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt  as GetOpt
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import           Text.Report.Types      (Report, runReport)

data Flag =
    GhcOptions [String]
    | PandocExe FilePath
    | PandocOptions [String]
    | FromFormat String
    | ToFormat String
    | OutputFile FilePath

data Arguments = Arguments
    { inputFile :: FilePath
    , flags     :: [Flag]
    }

options :: [OptDescr Flag]
options =
    [ Option "g" ["ghc-option"] (ReqArg (GhcOptions . pure) "OPTION")
        "An option for GHC"
    , Option "G" ["ghc-options"] (ReqArg (GhcOptions . words) "OPTIONs")
        "Several options for GHC (split on spaces)"
    , Option "p" ["pandoc-option"] (ReqArg (PandocOptions . pure) "OPTION")
        "An option for pandoc"
    , Option "P" ["pandoc-options"] (ReqArg (PandocOptions . words) "OPTIONs")
        "Several options for pandoc (split on spaces)"
    , Option "e" ["pandoc-path"] (ReqArg PandocExe "EXEC")
        "Where to find the pandoc executeable"
    , Option "f" ["--from"] (ReqArg FromFormat "FORMAT")
        "Input format (forwarded to pandoc)"
    , Option "t" ["--to"] (ReqArg ToFormat "FORMAT")
        "Output format (forwarded to pandoc)"
    , Option "o" ["--output"] (ReqArg OutputFile "FILE")
        "Output file (or - for stdout)"
    ]

getArguments :: IO Arguments
getArguments = do
    (flags', args, errors) <- GetOpt.getOpt GetOpt.Permute options <$> getArgs
    let header = "usage: haskell-report [OPTIONS] infile"
        info = GetOpt.usageInfo header options
        bail = putStrLn info >> exitFailure
    unless (null errors) $ do
        for_ errors putStrLn
        bail
    case args of
        [inputFile'] -> return $ Arguments inputFile' flags'
        _ -> bail

extractGhcArgs :: Arguments -> [String]
extractGhcArgs args = do
    f <- flags args
    case f of
        GhcOptions os -> os
        _ -> []

main :: IO ()
main = do
    args <- getArguments
    let interpOptions = Interp.Options
            { Interp.inputFile = inputFile args
            , Interp.ghcArgs   = extractGhcArgs args
            }
    Interp.runInterp interpOptions $ do
        report <- Interp.evalInline "stuff"
        inlines <- liftIO $ runReport report
        liftIO $ print inlines
