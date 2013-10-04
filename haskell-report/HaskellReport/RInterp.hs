{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HaskellReport.RInterp
    ( RInterp
    , Options (..)
    , evalInline, evalBlock
    , runRInterp
    ) where

import           Control.Monad          (forM_, unless)
import           Control.Monad.IO.Class
import           Data.Dynamic           (fromDyn, Typeable)
import qualified DynFlags
import           GHC                    (Ghc)
import qualified GHC
import           GHC.Paths              (libdir)
import qualified MonadUtils
import qualified Outputable
import qualified Text.Pandoc.Builder    as Pandoc
import           Text.Report.Types      (Report)

newtype RInterp a = RInterp (Ghc a)
    deriving (Monad)

instance MonadIO RInterp where
    liftIO = RInterp . MonadUtils.liftIO

data Options = Options
    { inputFile :: FilePath
    , ghcArgs   :: [String]
    }

evalBase :: Typeable a => String -> String -> RInterp a
evalBase f expr = RInterp $ do
    result <- GHC.dynCompileExpr $ "Text.Report.Types." ++ f ++ " (" ++ expr ++ ")"
    return $ fromDyn result $ error "Could not render expression"

evalInline :: String -> RInterp (Report Pandoc.Inlines)
evalInline = evalBase "render"

evalBlock :: String -> RInterp (Report Pandoc.Blocks)
evalBlock = evalBase "renderBlock"

runRInterp :: Options -> RInterp a -> IO a
runRInterp opts (RInterp act) =
    GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $
        GHC.runGhc (Just libdir) $ do
            dflags <- GHC.getSessionDynFlags
            GHC.defaultCleanupHandler dflags $ do
                parseGhcArguments $ ghcArgs opts
                loadFile $ inputFile opts
                importQualified "Text.Report.Types"
                act

-- Do the equivalent of a GHCi :load.
loadFile :: FilePath -> Ghc ()
loadFile file = do
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

printOutputables :: Outputable.Outputable a => [a] -> Ghc ()
printOutputables os = do
    dflags <- GHC.getSessionDynFlags
    let sdoc_to_string = Outputable.showSDocForUser dflags Outputable.neverQualify
    forM_ os $ MonadUtils.liftIO . putStrLn . sdoc_to_string . Outputable.ppr

-- Parses arguments for GHC, fails on unrecognized flags.
parseGhcArguments :: [String] -> Ghc ()
parseGhcArguments args = do
    dflags <- GHC.getSessionDynFlags
    let locatedArgs = map GHC.noLoc args
    (dflags', args', warnings) <- GHC.parseDynamicFlags dflags locatedArgs
    printOutputables warnings
    GHC.setSessionDynFlags dflags'
    let unlocatedArgs' = map GHC.unLoc args'
    unless (null args') $
        fail $ "Unrecognized arguments for GHC: " ++ unwords unlocatedArgs'
    return ()

-- Does a qualified import of a module into the interactive session.
importQualified :: String -> Ghc ()
importQualified m = do
    context <- GHC.getContext
    let
        -- A simple import declaration
        report_unqual = GHC.simpleImportDecl $ GHC.mkModuleName m
        -- Change it to a qualified import
        report_qual = report_unqual{ GHC.ideclQualified = True }
        report_interactive = GHC.IIDecl report_qual
    GHC.setContext $ report_interactive : context
