module Main where

import           Control.Monad       (forM_)
import           Data.Dynamic        (fromDyn)
import qualified DynFlags
import           GHC                 (Ghc)
import qualified GHC
import           GHC.Paths           (libdir)
import           MonadUtils          (liftIO)
import qualified Outputable
import           System.Environment  (getArgs)
import qualified Text.Pandoc.Builder as Pandoc
import           Text.Report.Types   (Report, runReport)

printOutputables :: Outputable.Outputable a => [a] -> Ghc ()
printOutputables os = do
    dflags <- GHC.getSessionDynFlags
    let sdoc_to_string = Outputable.showSDocForUser dflags Outputable.neverQualify
    forM_ os $ liftIO . putStrLn . sdoc_to_string . Outputable.ppr

-- Do the equivalent of a GHCi :load.
setupSession :: FilePath -> Ghc ()
setupSession file = do
    dflags <- GHC.getSessionDynFlags
    -- If we do not set the target to interpreted, the file has to have a main
    -- function or a proper 'module X where ...' header.
    let dflags' = dflags{
        GHC.ghcLink=GHC.LinkInMemory,
        GHC.hscTarget=GHC.HscInterpreted}
    GHC.setSessionDynFlags dflags'
    -- TODO, do not guess
    target <- GHC.guessTarget file Nothing
    GHC.addTarget target
    GHC.load GHC.LoadAllTargets
    -- The below two lines add the file to the interactive context
    graph <- GHC.depanal [] False
    GHC.setContext $ map (GHC.IIModule . GHC.ms_mod_name) graph

-- Does a qualified import of Text.Report.Types.
importReportTypes :: Ghc ()
importReportTypes = do
    context <- GHC.getContext
    let
        -- A simple import declaration
        report_unqual = GHC.simpleImportDecl $ GHC.mkModuleName "Text.Report.Types"
        -- Change it to a qualified import
        report_qual = report_unqual{ GHC.ideclQualified = True }
        report_interactive = GHC.IIDecl report_qual
    GHC.setContext $ report_interactive : context

-- Parses arguments for GHC, returns left over arguments.
parseGhcArguments :: [String] -> Ghc [String]
parseGhcArguments args = do
    dflags <- GHC.getSessionDynFlags
    let located_args = map GHC.noLoc args
    (dflags', args', warnings) <- GHC.parseDynamicFlags dflags located_args
    printOutputables warnings
    GHC.setSessionDynFlags dflags'
    return $ map GHC.unLoc args'

main :: IO ()
main = GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
        [infile] <- parseGhcArguments =<< liftIO getArgs
        setupSession infile
        importReportTypes
        result <- GHC.dynCompileExpr "Text.Report.Types.render stuff"
        let report = fromDyn result (undefined :: Report Pandoc.Inlines)
        inlines <- liftIO $ runReport report
        liftIO $ print inlines
