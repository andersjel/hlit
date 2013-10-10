module Main where

import           Control.Applicative
import           Control.Monad           (unless)
import qualified Data.Aeson              as Aeson
import           Data.Foldable
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           HaskellReport.Extract   (extract)
import qualified HaskellReport.Interp    as Interp
import           System.Console.GetOpt   (ArgDescr (..), OptDescr (..))
import qualified System.Console.GetOpt   as GetOpt
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)
import           System.Process          (readProcess)
import           Text.Pandoc             (Pandoc)
import           Text.Report.Types       (Report, runReport)

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

run :: Arguments -> IO ()
run args = do
    let pandocInArgs = getPandocArgs args PandocRead 
            ++ ["-t", "json", inputFile args]
    pandocInOutput <- readProcess (getPandocExe args) pandocInArgs ""
    let utf8PandocOutput = encodeUtf8 . T.strip . T.pack $ pandocInOutput
        eitherPandoc :: Either String Pandoc
        eitherPandoc = Aeson.eitherDecode' utf8PandocOutput
    pandoc <- case eitherPandoc of
        Left err -> putStrLn err >> exitFailure
        Right p -> return p
    let interpOptions = Interp.Options
            { Interp.inputFile = inputFile args
            , Interp.ghcArgs   = getGhcArgs args
            }
    report <- Interp.runInterp interpOptions $ extract pandoc
    pandoc' <- runReport report
    let pandocOutArgs =
            getPandocArgs args PandocWrite
            ++ ["-f", "json", "-o", getOutputFile args, "-"]
        jsonPandoc' = T.unpack . decodeUtf8 $ Aeson.encode pandoc'
    pandocOutOutput <- readProcess (getPandocExe args) pandocOutArgs jsonPandoc'
    putStr pandocOutOutput

main :: IO ()
main = run =<< getArguments
