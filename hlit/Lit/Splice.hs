module Lit.Splice
    ( Splice
    , Expr (..)
    , Error (..)
    , Options (..)
    , splice
    , runSplice
    ) where

import           Control.Applicative
import           Control.Monad.Error          (runErrorT, throwError)
import qualified Control.Monad.Error          as Err
import           Control.Monad.State.Strict   (StateT)
import qualified Control.Monad.State.Strict   as State
import qualified Data.Aeson                   as Aeson
import           Data.Data                    (Data, Typeable, cast, gmapT)
import           Data.Foldable                (toList)
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable
import qualified Language.Haskell.Exts        as H
import           Language.Haskell.Exts.SrcLoc (noLoc)

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
    deriving (Show)
data Options = Options
    { inputFilePath :: Maybe FilePath
    , inputContent  :: String
    , spliceImports :: [H.ImportDecl]
    , mainImports   :: [H.ImportDecl]
    , mainRun       :: H.Exp
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

data SpliceModules = SpliceModules
    { spliceMod :: H.Module
    , mainMod   :: H.Module
    }

-- | Transform immediate subterms if they have type b.
tmap :: (Data a, Typeable b) => (b -> b) -> a -> a
tmap f = gmapT g
  where
    g :: Typeable c => c -> c
    g t = fromMaybe t $ cast =<< (f <$> cast t)

withParseError :: H.ParseResult a -> Either Error a
withParseError (H.ParseOk x) = return x
withParseError (H.ParseFailed _ s) = throwError $ ParseError s

mkSpliceModules :: Options -> Splice a -> Either Error SpliceModules
mkSpliceModules opts (Splice exprs _) = do
    spliceModBase <- withParseError $ H.parseFileContents $ inputContent opts
    parsedExprs <- for exprs $ \(Expr typ expr) -> do
        expr' <- withParseError $ H.parse expr
        return (typ, expr' :: H.Exp)
    let names            = H.genNames "_hlit_splice" $ Seq.length exprs
        spliceModName    = H.ModuleName "HLitSpliceMod"
        spliceMod_       = setName . addSplices . addSpliceImports $ spliceModBase
        addSpliceImports = tmap (++ spliceImports opts)
        addSplices       = tmap (++ splices)
        setName          = tmap $ const spliceModName
        splices          = concatMap toSplice $ zip names (toList parsedExprs)
        toSplice (name, (typ, expr)) =
            [H.TypeSig noLoc [name] typ, H.nameBind noLoc name expr]
    mainModBase <- withParseError $ H.parse
        "module Main where\n\
        \import qualified HLitSpliceMod        as S\n\
        \import qualified Text.Aeson           as A\n\
        \import           Text.Aeson           (toJSON)\
        \import qualified Data.ByteString.Lazy as B\n\
        \import           Data.Applicative     (traverse)\n\
        \main = do\n\
        \  Just input <- A.decode <$> B.getContents\n\
        \  output <- mainRun input $ traverse splices\n\
        \  B.putStr $ A.encode output\n"
    let mainMod_       = addMainExpr . addMainImports $ mainModBase
        addMainExpr    = tmap (++ mainModDecls)
        addMainImports = tmap (++ mainImports opts)
        mainModDecls   =
            [ H.nameBind noLoc (H.name "mainRun") $ mainRun opts
            , H.nameBind noLoc (H.name "splices") $ H.listE $
                map (H.app (H.var $ H.name "toJSON")
                    . H.qvar spliceModName) names
            ]
    return $ SpliceModules spliceMod_ mainMod_

runSplice :: Options -> Splice a -> IO (Either Error a)
runSplice opts spl = runErrorT
    undefined
