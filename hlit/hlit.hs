{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import           Control.Monad                  (unless)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString.Lazy           (ByteString, hPut)
import           Data.Char                      (toUpper)
import           Data.Default
import           Data.Foldable                  (for_)
import           Data.Lens.Common
import           Data.Lens.Template
import           Data.Maybe                     (fromMaybe)
import qualified Language.Haskell.Exts          as H
import           Language.Haskell.Exts.SrcLoc   (noLoc)
import           Lit.Extract                    (Extraction (..), extract)
import qualified Lit.Splice                     as Splice
import           Lit.Splice                     (Splice)
import           System.Console.GetOpt          (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt          as GetOpt
import           System.Environment             (getArgs)
import           System.Exit                    (ExitCode (..), exitFailure)
import qualified System.IO                      as IO
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import           Text.Pandoc.Builder            (Pandoc)

data Arguments = Arguments
    { ghcOptions    :: [String]
    , inputFile     :: FilePath
    , inputFormat   :: Maybe String
    , outputFile    :: Maybe FilePath
    , outputFormat  :: Maybe String
    , pandocExe     :: FilePath
    , pandocOptions :: [String]
    }

nameMakeLens ''Arguments $ \(c:cs) -> Just $ 'l' : toUpper c : cs

instance Default Arguments where
    def = Arguments
        { ghcOptions = def
        , inputFile = def
        , inputFormat = def
        , outputFile = def
        , outputFormat = def
        , pandocExe = "pandoc"
        , pandocOptions = def
        }

options :: [OptDescr (Arguments -> Arguments)]
options =
    [ Option "g" ["ghc-option"]
        (ReqArg (\x -> modL lGhcOptions (++[x])) "OPTION")
        "An option for GHC"
    , Option "G" ["ghc-options"]
        (ReqArg (\x -> modL lGhcOptions (++words x)) "OPTIONs")
        "Several options for GHC (split on spaces)"
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
        "Output file (or - for stdout)"
    ]

getArguments :: IO Arguments
getArguments = do
    (flags, args, errors) <- GetOpt.getOpt GetOpt.Permute options <$> getArgs
    let header = "usage: hlit [OPTIONS] (INPUT-FILE or -)"
        info = GetOpt.usageInfo header options
        bail = putStrLn info >> exitFailure
    unless (null errors) $ do
        for_ errors putStrLn
        bail
    case args of
        [inputFile'] -> return $ foldl (flip id) def{inputFile = inputFile'} flags
        _ -> bail

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

runSplice :: Arguments -> H.Module -> Splice Pandoc -> IO Pandoc
runSplice args inputModule spl = do
    let qualImport x = H.ImportDecl noLoc
            (H.ModuleName x)
            True    -- ^ Qualified
            False   -- ^ With SOURCE pragma?
            Nothing -- ^ Package name
            Nothing -- ^ As ...
            Nothing -- ^ Import specs
        opt :: Splice.Options
        opt = def
            { Splice.inputFilePath = Just $ inputFile args
            , Splice.inputContent  = inputModule
            , Splice.spliceImports =
                [ qualImport "Text.Lit.Render"
                , qualImport "Data.Foldable"
                , qualImport "Text.Pandoc.Builder"
                ]
            , Splice.mainImports   = [qualImport "Text.Lit.Report"]
            , Splice.mainRun       = mainRunExpr
            , Splice.outputDir     = Nothing
            , Splice.ghcOptions    = ghcOptions args
            }
        H.ParseOk mainRunExpr = H.parse "\\() -> Text.Lit.Report.runReport"
    Splice.runSplice opt () spl >>= \r -> case r of
        Right x -> return x
        Left err -> print err >> fail "Conversion failed."

run :: Arguments -> IO ()
run args = do
    doc <- readDoc args
    let Extraction code spl = extract doc
    inputModule <- case H.parse code of
        H.ParseOk x -> return x
        failure -> fail $ show failure
    doc' <- runSplice args inputModule spl
    writeDoc args doc'

main :: IO ()
main = run =<< getArguments
