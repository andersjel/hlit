{-# LANGUAGE FlexibleContexts #-}

module Lit.Splice
    ( Splice
    , Error (..)
    , Options (..)
    , splice
    , runSplice
    ) where

import           Control.Applicative
import           Control.Monad.Error            (runErrorT, throwError)
import qualified Control.Monad.Error            as Err
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.State.Strict     (StateT)
import qualified Control.Monad.State.Strict     as State
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Data                      (Data, Typeable, cast, gmapT)
import           Data.Default
import           Data.Foldable                  (toList)
import qualified Data.Foldable                  as Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import qualified Language.Haskell.Exts          as H
import           Language.Haskell.Exts.SrcLoc   (noLoc)
import           System.Exit                    (ExitCode (..))
import           System.FilePath                (combine)
import qualified System.IO                      as IO
import           System.IO.Temp                 (withTempDirectory)
import qualified System.Process                 as Process
import           System.Process.ByteString.Lazy (readProcessWithExitCode)

data Splice a = Splice
    (Seq Expr) -- The expressions that need to be compiled
    (Load a)   -- Monad for loading the result of running each expression

instance Functor Splice where
    fmap f (Splice s a) = Splice s (f <$> a)

instance Applicative Splice where
    pure = Splice mempty . pure
    Splice sx x <*> Splice sy y = Splice (sx <> sy) (x <*> y)

data Expr = Expr H.Type H.Exp

data Error
    = CommunicationError String
    | MiscError String
    | CommandError String [String] Int
    deriving (Show)

data Options = Options
    { inputFilePath :: Maybe FilePath
    , inputContent  :: H.Module
    , spliceImports :: [H.ImportDecl]
    , mainImports   :: [H.ImportDecl]
    , mainRun       :: H.Exp
    , outputDir     :: Maybe FilePath
    , ghcOptions    :: [String]
    , ghcExe        :: FilePath
    }

instance Default Options where
    def = Options
        { inputFilePath = def
        , inputContent  = base
        , spliceImports = []
        , mainImports   = []
        , mainRun       = H.lamE noLoc [H.pTuple []] (H.function "id")
        , outputDir     = def
        , ghcOptions    = def
        , ghcExe        = "ghc"
        }
      where
        H.ParseOk base = H.parse "module SpliceBase where\n"

instance Err.Error Error where
    noMsg = Err.strMsg "Unknown error"
    strMsg = MiscError

type Load = StateT [Aeson.Value] (Either Error)

splice :: Aeson.FromJSON a => H.Type -> H.Exp -> Splice a
splice typ expr = Splice (Seq.singleton $ Expr typ expr) $ do
    (v:vs) <- State.get
    State.put vs
    case Aeson.fromJSON v of
        Aeson.Success x -> return x
        Aeson.Error x -> throwError $ CommunicationError x

-- | Transform immediate subterms if they have type b.
tmap :: (Data a, Typeable b) => (b -> b) -> a -> a
tmap f = gmapT g
  where
    g :: Typeable c => c -> c
    g t = fromMaybe t $ cast =<< (f <$> cast t)

fromEither :: Err.MonadError a m => Either a b -> m b
fromEither (Right x) = return x
fromEither (Left x) = throwError x

mkSpliceNames :: Int -> [H.Name]
mkSpliceNames = H.genNames "_hlit_splice"

exportSpec :: H.Name -> H.ExportSpec
#if MIN_VERSION_haskell_src_exts(1,16,0)
exportSpec = H.EVar H.NoNamespace . H.UnQual
#else
exportSpec = H.EVar . H.UnQual
#endif

spliceModName :: H.ModuleName
spliceModName = H.ModuleName "HLitSpliceMod"

mkSpliceMod :: Options -> Splice a -> H.Module
mkSpliceMod opts (Splice exprs _)
    = tmap setName
    . tmap addExports . tmap addSpliceImports . tmap addSplices
    $ inputContent opts
  where
    setName          = const spliceModName
    names            = mkSpliceNames $ Seq.length exprs
    addExports :: Maybe [H.ExportSpec] -> Maybe [H.ExportSpec]
    addExports xs    = Just $ Foldable.concat xs ++ map exportSpec names
    addSpliceImports = (++ spliceImports opts)
    addSplices       = (++ splices)
    splices          = concat $ zipWith toSplice names (toList exprs)
    toSplice name (Expr typ expr) =
        [ H.TypeSig noLoc [name] typ
        , H.nameBind noLoc name expr]

mkMainMod :: Options -> Splice b -> H.Module
mkMainMod opts (Splice exprs _) = mainMod
  where
    H.ParseOk mainModBase = H.parse $ unlines
        [ "module Main where"
        , "import qualified HLitSpliceMod"
        , "import qualified Data.Aeson           as A"
        , "import           Data.Aeson           (toJSON)"
        , "import qualified Data.ByteString.Lazy as B"
        , "main = do"
        , "    Just input <- A.decode `fmap` B.getContents"
        , "    output <- mainRun input $ sequence splices"
        , "    B.putStr $ A.encode output"
        , ""]
    names          = mkSpliceNames $ Seq.length exprs
    mainMod        = addMainExpr . addMainImports $ mainModBase
    addMainExpr    = tmap (++ mainModDecls)
    addMainImports = tmap (++ mainImports opts)
    mainModDecls   =
        [ H.nameBind noLoc (H.name "mainRun") $ mainRun opts
        , H.nameBind noLoc (H.name "splices") $ H.listE $
            map (H.app (H.app (H.function "fmap") (H.function "toJSON"))
                . H.qvar spliceModName) names
        ]

withOutputDir :: Options -> (FilePath -> IO a) -> IO a
withOutputDir opts act =
    case outputDir opts of
        Just d -> act d
        Nothing -> withTempDirectory "." "splice." act

checkCallSilent
    :: (Err.MonadError Error m, MonadIO m)
    => FilePath -> [String] -> m ()
checkCallSilent com args = do
    let c = Process.proc com args
        c' = c{Process.std_out=Process.UseHandle IO.stderr}
    (_, _, _, h) <- liftIO $ Process.createProcess c'
    exitCode <- liftIO $ Process.waitForProcess h
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure n -> throwError $ CommandError com args n

checkProcess
    :: (Err.MonadError Error m, MonadIO m)
    => FilePath         -- ^ Command to run
    -> [String]         -- ^ Arguments
    -> BL.ByteString    -- ^ Standard input
    -> m BL.ByteString  -- ^ Standard output
checkProcess com args stdin = do
    (exitCode, stdout, stderr) <- liftIO $
        readProcessWithExitCode com args stdin
    liftIO $ BL.hPutStr IO.stderr stderr
    case exitCode of
        ExitSuccess   -> return stdout
        ExitFailure n -> throwError $ CommandError com args n

runSplice
    :: (Aeson.ToJSON b)
    => Options
    -> b
    -> Splice a
    -> IO (Either Error a)
runSplice opts arg spl@(Splice _ loader)
    = withOutputDir opts $ \outDir -> runErrorT $ do
        let
            spliceMod = mkSpliceMod opts spl
            mainMod   = mkMainMod opts spl
            storeMod path m = liftIO $
                IO.withFile path IO.WriteMode $ \h ->
                    IO.hPutStrLn h $ H.prettyPrint m
            spliceModPath = combine outDir "HLitSpliceMod.hs"
            mainModPath = combine outDir "HLitSpliceMain.hs"
            mainPath = combine outDir "HLitSpliceMain"
        storeMod spliceModPath spliceMod
        storeMod mainModPath mainMod
        checkCallSilent (ghcExe opts) $
            ghcOptions opts ++
            ["-outputdir", outDir, "-i" ++ outDir, spliceModPath]
        checkCallSilent (ghcExe opts) $
            ghcOptions opts ++
            ["-outputdir", outDir, "-i" ++ outDir, "-o", mainPath, mainModPath]
        output <- checkProcess mainPath [] $ Aeson.encode arg
        splices <- case Aeson.eitherDecode output of
            Left x  -> throwError $ CommunicationError x
            Right x -> return (x :: [Aeson.Value])
        fromEither $ State.evalStateT loader splices
