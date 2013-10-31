{-# LANGUAGE FlexibleContexts #-}

module Lit.Splice
    ( Splice
    , Expr (..)
    , Error (..)
    , Options (..)
    , splice
    , runSplice

    , tests
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
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import           Data.Traversable
import qualified Language.Haskell.Exts          as H
import           Language.Haskell.Exts.SrcLoc   (noLoc)
import           System.Exit                    (ExitCode (..))
import           System.FilePath                (combine)
import qualified System.IO                      as IO
import           System.IO.Temp                 (withTempDirectory)
import qualified System.Process                 as Process
import           System.Process.ByteString.Lazy (readProcessWithExitCode)

import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as HU

data Splice a = Splice
    (Seq Expr) -- ^ The expressions that need to be compiled
    (Load a)   -- ^ Monad for loading the result of running each expression

instance Functor Splice where
    fmap f (Splice s a) = Splice s (f <$> a)

instance Applicative Splice where
    pure = Splice mempty . pure
    Splice sx x <*> Splice sy y = Splice (sx <> sy) (x <*> y)

data Expr = Expr H.Type String
data Error
    = CommunicationError String
    | MiscError String
    | ParseError String
    | CommandError String [String] Int
    deriving (Show)
data Options = Options
    { inputFilePath :: Maybe FilePath
    , inputContent  :: String
    , spliceImports :: [H.ImportDecl]
    , mainImports   :: [H.ImportDecl]
    , mainRun       :: H.Exp
    , outputDir     :: Maybe FilePath
    }

instance Default Options where
    def = Options
        { inputFilePath = def
        , inputContent  = "module SpliceBase where\n"
        , spliceImports = []
        , mainImports   = []
        , mainRun       = H.lamE noLoc [H.pTuple []] (H.function "id")
        , outputDir     = def
        }

instance Err.Error Error where
    noMsg = Err.strMsg "Unknown error"
    strMsg = MiscError

type Load = StateT [Aeson.Value] (Either Error)

splice :: Aeson.FromJSON a => Expr -> Splice a
splice expr = Splice (Seq.singleton expr) $ do
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

fromParseError :: H.ParseResult a -> Either Error a
fromParseError (H.ParseOk x) = return x
fromParseError (H.ParseFailed _ s) = throwError $ ParseError s

fromEither :: Err.MonadError a m => Either a b -> m b
fromEither (Right x) = return x
fromEither (Left x) = throwError x

mkSpliceNames :: Int -> [H.Name]
mkSpliceNames = H.genNames "_hlit_splice"

spliceModName :: H.ModuleName
spliceModName = H.ModuleName "HLitSpliceMod"

mkSpliceMod :: Options -> Splice a -> Either Error H.Module
mkSpliceMod opts (Splice exprs _) = do
    spliceModBase <- fromParseError $ H.parseFileContents $ inputContent opts
    parsedExprs <- for exprs $ \(Expr typ expr) -> do
        expr' <- fromParseError $ H.parse expr
        return (typ, expr' :: H.Exp)
    let names            = mkSpliceNames $ Seq.length exprs
        spliceMod       
            = tmap setName 
            . tmap addExports 
            . tmap addSpliceImports 
            . tmap addSplices 
            $ spliceModBase
        setName          = const spliceModName
        addExports :: Maybe [H.ExportSpec] -> Maybe [H.ExportSpec]
        addExports       = fmap (++ map (H.EVar . H.UnQual) names)
        addSpliceImports = (++ spliceImports opts)
        addSplices       = (++ splices)
        splices          = concatMap toSplice $ zip names (toList parsedExprs)
        toSplice (name, (typ, expr)) =
            [ H.TypeSig noLoc [name] typ
            , H.nameBind noLoc name expr]
    return spliceMod

mkMainMod :: Options -> Splice b -> Either Error H.Module
mkMainMod opts (Splice exprs _) = do
    mainModBase <- fromParseError $ H.parse
        "module Main where\n\
        \import qualified HLitSpliceMod\n\
        \import qualified Data.Aeson           as A\n\
        \import           Data.Aeson           (toJSON)\n\
        \import qualified Data.ByteString.Lazy as B\n\
        \main = do\n\
        \  Just input <- A.decode `fmap` B.getContents\n\
        \  output <- mainRun input $ sequence splices\n\
        \  B.putStr $ A.encode output\n"
    let names          = mkSpliceNames $ Seq.length exprs
        mainMod       = addMainExpr . addMainImports $ mainModBase
        addMainExpr    = tmap (++ mainModDecls)
        addMainImports = tmap (++ mainImports opts)
        mainModDecls   =
            [ H.nameBind noLoc (H.name "mainRun") $ mainRun opts
            , H.nameBind noLoc (H.name "splices") $ H.listE $
                map (H.app (H.app (H.function "fmap") (H.function "toJSON"))
                    . H.qvar spliceModName) names
            ]
    return mainMod

withOutputDir :: Options -> (FilePath -> IO a) -> IO a
withOutputDir opts act =
    case outputDir opts of
        Just d -> act d
        Nothing -> withTempDirectory "." "splice." act

checkCall :: (Err.MonadError Error m, MonadIO m) => FilePath -> [String] -> m ()
checkCall com args = do
    exitCode <- liftIO $ Process.rawSystem com args
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure n -> throwError $ CommandError com args n

checkProcess 
    :: (Err.MonadError Error m, MonadIO m) 
    => FilePath         -- ^ Command to run
    -> [String]         -- ^ Arguments
    -> BL.ByteString    -- ^ Standard input
    -> m BL.ByteString -- ^ Standard output
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
        spliceMod <- fromEither $ mkSpliceMod opts spl
        mainMod   <- fromEither $ mkMainMod opts spl
        let storeMod path m = liftIO $ 
                IO.withFile path IO.WriteMode $ \h ->
                    IO.hPutStrLn h $ H.prettyPrint m
            spliceModPath = combine outDir "HLitSpliceMod.hs"
            mainModPath = combine outDir "HLitSpliceMain.hs"
            mainPath = combine outDir "HLitSpliceMain"
        storeMod spliceModPath spliceMod
        storeMod mainModPath mainMod
        checkCall "ghc" 
            ["-outputdir", outDir, "-i" ++ outDir, spliceModPath]
        checkCall "ghc" 
            ["-outputdir", outDir, "-i" ++ outDir, "-o", mainPath, mainModPath]
        output <- checkProcess mainPath [] $ Aeson.encode arg
        splices <- case Aeson.eitherDecode output of
            Left x  -> throwError $ CommunicationError x
            Right x -> return (x :: [Aeson.Value])
        fromEither $ State.evalStateT loader splices

tests :: Test
tests = testGroup "Splice"
    [ testCase "basic_splice" case_basic_splice
    ]

case_basic_splice :: HU.Assertion
case_basic_splice = do
    let typ = H.TyApp (t "IO") (t "Int")
        t = H.TyCon . H.UnQual . H.name
    r <- runSplice def{outputDir=Just "splices"} () $ 
        (+) <$> pure (2 :: Int) <*> splice (Expr typ "return $ 1 + 2")
    v <- case r of 
        Left x -> error $ show x
        Right x -> return x
    v HU.@?= 5
