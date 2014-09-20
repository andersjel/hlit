{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lit where

import           Control.Applicative
import           Control.Monad                  (unless, when)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString.Lazy           (ByteString, hPut)
import           Data.Char                      (toUpper)
import           Data.Default
import           Data.Foldable                  (for_, traverse_)
import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Maybe                     (fromMaybe, isJust)
import qualified Language.Haskell.Exts          as H
import qualified Lit.MkDoc                      as MkDoc
import qualified Lit.Splice                     as Splice
import           System.Console.GetOpt          (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt          as GetOpt
import           System.Environment             (getArgs)
import           System.Directory               (createDirectoryIfMissing)
import           System.Exit                    (ExitCode (..), exitFailure)
import           System.FilePath                (replaceFileName)
import qualified System.IO                      as IO
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Text.Lit.Report                as Report
import           Text.Pandoc.Builder            (Pandoc)

data Arguments = Arguments
    { ghcOptions    :: [String]
    , inputFile     :: FilePath
    , inputFormat   :: Maybe String
    , outputFile    :: Maybe FilePath
    , outputFormat  :: Maybe String
    , mediaFolder   :: Maybe String
    , pandocExe     :: FilePath
    , ghcExe        :: FilePath
    , pandocOptions :: [String]
    , tmpFolder     :: Maybe FilePath
    }

nameMakeLens ''Arguments $ \(c:cs) -> Just $ 'l' : toUpper c : cs

instance Default Arguments where
    def = Arguments
        { ghcOptions = def
        , inputFile = def
        , inputFormat = def
        , outputFile = def
        , outputFormat = def
        , mediaFolder = def
        , pandocExe = "pandoc"
        , ghcExe = "ghc"
        , pandocOptions = def
        , tmpFolder = def
        }

options :: [OptDescr (Arguments -> Arguments)]
options =
    [ Option "g" ["ghc-option"]
        (ReqArg (\x -> modL lGhcOptions (++[x])) "OPTION")
        "An option for GHC"
    , Option "G" ["ghc-options"]
        (ReqArg (\x -> modL lGhcOptions (++words x)) "OPTIONs")
        "Several options for GHC (split on spaces)"
    , Option "" ["ghc-path"]
        (ReqArg (setL lGhcExe) "EXEC")
        "Where to find the ghc executeable"
    , Option "p" ["pandoc-option"]
        (ReqArg (\x -> modL lPandocOptions (++[x])) "OPTION")
        "An option for pandoc"
    , Option "P" ["pandoc-options"]
        (ReqArg (\x -> modL lPandocOptions (++words x)) "OPTIONs")
        "Several options for pandoc (split on spaces)"
    , Option "" ["pandoc-path"]
        (ReqArg (setL lPandocExe) "EXEC")
        "Where to find the pandoc executeable"
    , Option "f" ["from"]
        (ReqArg (setL lInputFormat . Just) "FORMAT")
        "Input format (forwarded to pandoc)"
    , Option "t" ["to"]
        (ReqArg (setL lOutputFormat . Just) "FORMAT")
        "Output format (forwarded to pandoc)"
    , Option "o" ["output"]
        (ReqArg (setL lOutputFile . Just) "FILE")
        "Output file (omit for stdout)"
    , Option "m" ["media"]
        (ReqArg (setL lMediaFolder . Just) "NAME")
        "Name of a folder for images and such\n(only letters from a-z)"
    , Option "" ["tmp"]
        (ReqArg (setL lTmpFolder . Just) "PATH") $ unlines
        [ "Store temporary generated Haskell code and"
        , "executeables in this folder (and leave them"
        , "there after the program terminates)"]
    ]

getArguments :: IO Arguments
getArguments = do
    let header = "usage: hlit [OPTIONS] (INPUT-FILE or -)"
        info = GetOpt.usageInfo header options
        bail = putStrLn info >> exitFailure
    args <- parseArguments <$> getArgs
    case args of
        Right x -> return x
        Left errors -> for_ errors putStrLn >> bail

parseArguments :: [String] -> Either [String] Arguments
parseArguments args = do
    let (flags, args', errors) = GetOpt.getOpt GetOpt.Permute options args
        arguments = foldl (flip id) def flags
    unless (null errors) $ Left errors
    when (isJust $ mediaFolder arguments) $
        unless (isJust $ outputFile arguments) $
            Left ["stdout cannot be used for output when "
                ++ "an output folder is specified (use -o FILE or "
                ++ "omit --output-folder FOLDER-NAME)"]
    case args' of
        [inputFile'] -> Right arguments{inputFile=inputFile'}
        _ -> Left ["Missing argument: INPUT-FILE"]

callProcess :: FilePath -> [String] -> ByteString -> IO ByteString
callProcess exe args stdin = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode exe args stdin
    hPut IO.stderr stderr
    case exitCode of
        ExitSuccess -> return stdout
        _ -> fail "Process exited with a failure."

readDoc :: Arguments -> IO Pandoc
readDoc args = do
    let args' =
            pandocOptions args
            ++ maybe [] (\f -> ["-f", f]) (inputFormat args)
            ++ ["-t", "json", inputFile args]
    output <- callProcess (pandocExe args) args' ""
    case Aeson.decode' output of
        Nothing -> do
            putStrLn "Unexpected output from pandoc:"
            hPut IO.stderr output
            exitFailure
        Just p -> return p

writeDoc :: Arguments -> Pandoc -> IO ()
writeDoc args doc = do
    let args' =
            pandocOptions args
            ++ maybe [] (\t -> ["-t", t]) (outputFormat args)
            ++ ["-o", fromMaybe "-"  $ outputFile args]
            ++ ["-f", "json", "-"]
    output <- callProcess (pandocExe args) args' $ Aeson.encode doc
    hPut IO.stdout output

run :: Arguments -> IO ()
run args = do
    doc <- readDoc args
    let code = MkDoc.extractCode doc
    inputModule <- case H.parse code of
        H.ParseOk x -> return x
        failure -> fail $ show failure
    let splice = MkDoc.extractSplice doc
        sopt = MkDoc.docSpliceOptions
            { Splice.inputFilePath = Just $ inputFile args
            , Splice.inputContent  = inputModule
            , Splice.ghcOptions    = ghcOptions args
            , Splice.ghcExe        = ghcExe args
            , Splice.outputDir     = tmpFolder args
            }
        ropt = Report.Options
            $ mkOutputOptions
                <$> outputFile args
                <*> mediaFolder args
        mkOutputOptions p n =
            Report.OutputOptions (replaceFileName p n) (n ++ "/")
    -- TODO: mediaFolder is the name of a folder relative to the output
    -- document, not a path relative to cwd.
    for_ [tmpFolder args, mediaFolder args] $
        -- traverse the Maybes, create the folders, don't create parent dirs.
        traverse_ $ createDirectoryIfMissing False
    doc' <- Splice.runSplice sopt ropt splice >>= \r -> case r of
        Right x -> return x
        Left err -> print err >> fail "Conversion failed."
    writeDoc args doc'
