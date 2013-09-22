module Main where

import           Data.Dynamic       (fromDyn)
import qualified DynFlags
import           GHC                (Ghc)
import qualified GHC
import           GHC.Paths          (libdir)
import           MonadUtils         (liftIO)
import           System.Environment (getArgs)

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
    GHC.setContext $ map (GHC.IIModule . GHC.ms_mod_name) graph

main :: IO ()
main = GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
        [infile] <- liftIO getArgs
        loadFile infile
        result <- GHC.dynCompileExpr "show stuff"
        liftIO $ putStrLn $ fromDyn result (undefined :: String)
