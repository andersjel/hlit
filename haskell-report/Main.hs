module Main where

import           Control.Applicative
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

-- Also sets the context to include everything at the top level of file
loadFile :: FilePath -> Ghc ()
loadFile file = do
    dflags <- GHC.getSessionDynFlags
    -- If we do not set the target to interpreted, the file has to have a main
    -- function or a proper 'module X where ...' header.
    let dflags' = dflags{
        GHC.ghcLink=GHC.LinkInMemory,
        GHC.hscTarget=GHC.HscInterpreted}
    _ <- GHC.setSessionDynFlags dflags'
    -- TODO, do not guess
    target <- GHC.guessTarget file Nothing
    GHC.addTarget target
    graph <- GHC.depanal [] False
    _ <- GHC.load GHC.LoadAllTargets
    let
        -- The file itself has to be imported
        imp_file = map (GHC.IIModule . GHC.ms_mod_name) graph
        -- We also need Text.Report.Types
        imp_report_promiscuous = GHC.simpleImportDecl $ GHC.mkModuleName "Text.Report.Types"
        imp_report = imp_report_promiscuous{ GHC.ideclQualified = True }
        imports = GHC.IIDecl imp_report : imp_file
    GHC.setContext imports

-- Parses GHC arguments, returns left over arguments.
parseGhcArguments :: Ghc [String]
parseGhcArguments = do
    dflags <- GHC.getSessionDynFlags
    args <- map GHC.noLoc <$> liftIO getArgs
    (dflags', args', warnings) <- GHC.parseDynamicFlags dflags args
    -- Print each warning
    forM_ warnings $
        liftIO . putStrLn .
        Outputable.showSDocForUser dflags' Outputable.neverQualify .
        Outputable.ppr
    _ <- GHC.setSessionDynFlags dflags'
    return $ map GHC.unLoc args'

main :: IO ()
main = GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
        [infile] <- parseGhcArguments
        loadFile infile
        result <- GHC.dynCompileExpr "Text.Report.Types.render stuff"
        let report = fromDyn result (undefined :: Report Pandoc.Inlines)
        inlines <- liftIO $ runReport report
        liftIO $ print inlines
