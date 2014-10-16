{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lit where

import           Control.Applicative
import           Control.Monad                  (unless)
import qualified Data.Aeson                     as Aeson
import           Data.ByteString.Lazy           (ByteString, hPut)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toUpper)
import           Data.Default
import           Data.Foldable                  (for_)
import           Data.Label                     (modify, set)
import           Data.Label.Derive              (mkLabelsNamed)
import           Data.List                      (intercalate)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text.Lazy                 as TL
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import qualified Lit.DocSplice                  as DocSplice
import           Lit.ErrorHandling              (context, litThrowIO')
import qualified Lit.ErrorHandling
import qualified Lit.Extract                    as Extract
import qualified Lit.Splice                     as Splice
import qualified Network.URI                    as URI
import           System.Console.GetOpt          (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt          as GetOpt
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment             (getArgs)
import           System.Exit                    (ExitCode (..), exitFailure)
import           System.FilePath                ((</>))
import qualified System.FilePath                as FilePath
import qualified System.IO                      as IO
import           System.IO.Temp                 (withTempDirectory)
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Text.Lit.Report                as Report
import           Text.Pandoc.Builder            (Pandoc)

data Arguments = Arguments
    { ghcOptions    :: [String]
    , inputFile     :: Maybe FilePath
    , inputFormat   :: Maybe String
    , outputFile    :: Maybe FilePath
    , outputFormat  :: Maybe String
    , mediaFolder   :: Maybe String
    , pandocExe     :: FilePath
    , ghcExe        :: FilePath
    , pandocOptions :: [String]
    , tmpFolder     :: Maybe FilePath
    , mode          :: Extract.Mode
    }

mkLabelsNamed (\(c:cs) -> 'l' : toUpper c : cs) [''Arguments]

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
        , mode = Extract.Merge
        }

options :: [OptDescr (Arguments -> Arguments)]
options =
    [ Option "g" ["ghc-option"]
        (ReqArg (\x -> modify lGhcOptions (++[x])) "OPTION")
        "An option for GHC"
    , Option "G" ["ghc-options"]
        (ReqArg (\x -> modify lGhcOptions (++words x)) "OPTIONs")
        "Several options for GHC (split on spaces)"
    , Option "" ["ghc-path"]
        (ReqArg (set lGhcExe) "EXEC")
        "Where to find the ghc executeable"
    , Option "p" ["pandoc-option"]
        (ReqArg (\x -> modify lPandocOptions (++[x])) "OPTION")
        "An option for pandoc"
    , Option "P" ["pandoc-options"]
        (ReqArg (\x -> modify lPandocOptions (++words x)) "OPTIONs")
        "Several options for pandoc (split on spaces)"
    , Option "" ["pandoc-path"]
        (ReqArg (set lPandocExe) "EXEC")
        "Where to find the pandoc executeable"
    , Option "f" ["from"]
        (ReqArg (set lInputFormat . Just) "FORMAT")
        "Input format (forwarded to pandoc)"
    , Option "t" ["to"]
        (ReqArg (set lOutputFormat . Just) "FORMAT")
        "Output format (forwarded to pandoc)"
    , Option "m" ["mode"]
        (ReqArg (set lMode . f) "MODE") $ unlines
        [ "Mode of operation. Must be one of 'merge', 'import',"
        , "or 'explicit'. The default is 'merge'."]
    , Option "o" ["output"]
        (ReqArg (set lOutputFile . Just) "FILE")
        "Output file (omit for stdout)"
    , Option "e" ["media"]
        (ReqArg (set lMediaFolder . Just) "PATH")
        "Path to a folder to use for generated images and media"
    , Option "" ["tmp"]
        (ReqArg (set lTmpFolder . Just) "PATH") $ unlines
        [ "Store temporary generated Haskell code and"
        , "executeables in this folder (and leave them"
        , "there after the program terminates)"]
    ]
  where
    f "merge"    = Extract.Merge
    f "explicit" = Extract.Explicit
    f "import"   = Extract.Import
    f x = error $ "No such mode '" ++ x ++ "'"

getArguments :: IO Arguments
getArguments = do
    let header = "usage: hlit [OPTIONS] [INPUT-FILE]"
        info = GetOpt.usageInfo header options
        bail = IO.hPutStrLn IO.stderr info >> exitFailure
    args <- parseArguments <$> getArgs
    case args of
        Right x -> return x
        Left errors -> for_ errors (IO.hPutStrLn IO.stderr) >> bail

parseArguments :: [String] -> Either [String] Arguments
parseArguments args = do
    let (flags, args', errors) = GetOpt.getOpt GetOpt.Permute options args
        arguments = foldl (flip id) def flags
    unless (null errors) $ Left errors
    case args' of
        [inputFile'] -> Right arguments{inputFile=Just inputFile'}
        [] -> Right arguments
        _ -> Left ["Too many positional arguments"]

callProcess :: FilePath -> [String] -> ByteString -> IO ByteString
callProcess exe args stdin =
    context ("running: '" ++ intercalate " " (exe:args) ++ "'") $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode exe args stdin
        hPut IO.stderr stderr
        case exitCode of
            ExitSuccess -> return stdout
            _ -> fail $ "'" ++ exe ++ "' exited with a failure."

readDoc :: Arguments -> IO Pandoc
readDoc args = context "parsing input" $ do
    let args' =
            pandocOptions args
            ++ maybe [] (\x -> ["-f", x]) (inputFormat args)
            ++ ["-t", "json"]
            -- We convert x to ./x so that we do not have to care about files
            -- starting in '-'
            ++ maybe [] (\x -> ["." </> x]) (inputFile args)
    stdin <- case inputFile args of
        Nothing -> BL.getContents
        _       -> return ""
    output <- callProcess (pandocExe args) args' stdin
    case Aeson.decode' output of
        Nothing -> do
            litThrowIO' "Unexpected output from pandoc." $
                ("output was:\n" ++) . TL.unpack $ decodeUtf8 output
        Just p -> return p

writeDoc :: Arguments -> Pandoc -> IO ()
writeDoc args doc = do
    let args' =
            pandocOptions args
            ++ maybe [] (\x -> ["-t", x]) (outputFormat args)
            -- We convert x to ./x so that we do not have to care about files
            -- starting in '-'
            ++ maybe [] (\x -> ["-o", "." </> x]) (outputFile args)
            ++ ["-f", "json"]
    output <- callProcess (pandocExe args) args' $ Aeson.encode doc
    hPut IO.stdout output

withDir :: String -> Maybe FilePath -> (FilePath -> IO a) -> IO a
withDir template Nothing act = withTempDirectory "." template act
withDir _ (Just d) act = do
    createDirectoryIfMissing False d
    act d

setupMediaFolder :: Arguments -> FilePath -> IO (FilePath, String)
setupMediaFolder args tmp = do
    let path = fromMaybe (tmp </> "media") $ mediaFolder args
        base = case inputFile args of
            Nothing -> "."
            Just p  -> FilePath.dropFileName p
        relative = FilePath.makeRelative base path
        url = intercalate "/" $ map escape components
          where
            components = FilePath.splitDirectories relative
            escape = URI.escapeURIString URI.isUnescapedInURIComponent
    createDirectoryIfMissing False path
    return (path, url)

run :: Arguments -> IO ()
run args = Lit.ErrorHandling.funnel $ do
    doc <- readDoc args
    inputModule <- case Extract.extract (mode args) doc of
        Right m -> return m
        Left err -> fail err
    withDir "hlit." (tmpFolder args) $ \tmp -> do
        (mediaPath, mediaUrl) <- setupMediaFolder args tmp
        let splice = DocSplice.mk doc
            spliceOpt = DocSplice.options
                { Splice.inputFilePath = inputFile args
                , Splice.inputContent  = inputModule
                , Splice.ghcOptions    = ghcOptions args
                , Splice.ghcExe        = ghcExe args
                , Splice.outputDir     = Just tmp
                }
            outputOpt = Report.OutputOptions mediaPath mediaUrl
            reportOpt = Report.Options $ Just outputOpt
        result <- Splice.runSplice spliceOpt reportOpt splice
        either (fail . show) (writeDoc args) result
