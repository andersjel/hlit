{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad                  (unless)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString.Lazy           (ByteString, hPut)
import           Data.Foldable
import           HaskellReport.Extract          (extract)
import qualified HaskellReport.Interp           as Interp
import           System.Console.GetOpt          (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt          as GetOpt
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure, ExitCode(..))
import qualified System.IO as IO
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import           Text.Pandoc                    (Pandoc)
import           Text.Report.Types              (Report, runReport)

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

getGhcArgs :: Arguments -> [String]
getGhcArgs args = do
    f <- flags args
    case f of
        GhcOptions os -> os
        _ -> []

data PandocStage = PandocRead | PandocWrite

getPandocArgs :: Arguments -> PandocStage -> [String]
getPandocArgs args stage = do
    f <- flags args
    case f of
        PandocOptions os -> os
        ToFormat x -> case stage of
            PandocWrite -> ["-t", x]
            PandocRead -> []
        FromFormat x -> case stage of
            PandocWrite -> []
            PandocRead -> ["-f", x]
        _ -> []

getPandocExe :: Arguments -> String
getPandocExe args = g $ flags args
  where
    g [] = "pandoc"
    g (PandocExe exe : _) = exe
    g (_ : fs) = g fs

getOutputFile :: Arguments -> String
getOutputFile args = g $ flags args
  where
    g [] = "-"
    g (OutputFile path : _) = path
    g (_ : fs) = g fs

callProcess :: FilePath -> [String] -> ByteString -> IO ByteString
callProcess exe args stdin = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode exe args stdin
    hPut IO.stderr stderr
    case exitCode of
        ExitSuccess -> return stdout
        _ -> fail "Process exited with a failure."

readDoc :: Arguments -> IO Pandoc
readDoc args = do
    let args' = getPandocArgs args PandocRead
            ++ ["-t", "json", inputFile args]
    output <- callProcess (getPandocExe args) args' ""
    let doc :: Either String Pandoc
        doc = Aeson.eitherDecode' output
    case doc of
        Left err -> putStrLn err >> exitFailure
        Right p -> return p

writeDoc :: Arguments -> Pandoc -> IO ()
writeDoc args doc = do
    let args' = getPandocArgs args PandocWrite
            ++ ["-f", "json", "-o", getOutputFile args, "-"]
    output <- callProcess (getPandocExe args) args' $ Aeson.encode doc
    hPut IO.stdout output

extractReport :: Arguments -> Pandoc -> IO (Report Pandoc)
extractReport args doc = do
    let interpOptions = Interp.Options
            { Interp.inputFile = inputFile args
            , Interp.ghcArgs   = getGhcArgs args
            }
    Interp.runInterp interpOptions $ extract doc

run :: Arguments -> IO ()
run args = do
    doc <- readDoc args
    report <- extractReport args doc
    doc' <- runReport report
    writeDoc args doc'

main :: IO ()
main = run =<< getArguments
