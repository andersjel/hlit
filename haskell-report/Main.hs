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

data Arguments = Arguments
    { inputFile :: FilePath
    , flags     :: [Flag]
    }

options :: [OptDescr Flag]
options =
    [ Option "g" ["ghc-option"] (ReqArg (GhcOptions . pure) "OPT")
        "An option for GHC"
    ]

getArguments :: IO Arguments
getArguments = do
    (flags', args, errors) <- GetOpt.getOpt GetOpt.Permute options <$> getArgs
    let header = "haskell-report [OPTIONS] infile:"
        info = GetOpt.usageInfo header options
        bail = putStrLn info >> exitFailure
    unless (null errors) $ do
        for_ errors putStrLn
        putStrLn ""
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
