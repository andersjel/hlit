module Lit.Splice
    ( Splice
    , Expr (..)
    , Error
    , Options (..)
    , splice
    , runSplice
    ) where

import           Control.Applicative
import           Control.Monad.Error        (ErrorT)
import qualified Control.Monad.Error        as Err
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson                 as Aeson
import           Data.Monoid
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import qualified Language.Haskell.Exts      as H

data Splice a = Splice
    (Seq Expr) -- ^ The expressions that need to be compiled
    (Load a)   -- ^ Monad for loading the result of running each expression

instance Functor Splice where
    fmap f (Splice s a) = Splice s (f <$> a)

instance Applicative Splice where
    pure = Splice mempty . pure
    Splice sx x <*> Splice sy y = Splice (sx <> sy) (x <*> y)

data Expr = Expr H.Type String
data Error = CommunicationError String | MiscError String
    deriving (Show)
data Options = Options
    { inputFilePath :: Maybe FilePath
    , inputContent  :: String
    }

instance Err.Error Error where
    noMsg = Err.strMsg "Unknown error"
    strMsg = MiscError

type Load = ErrorT Error (State [Aeson.Value])

splice :: Aeson.FromJSON a => Expr -> Splice a
splice expr = Splice (Seq.singleton expr) $ do
    (v:vs) <- State.get
    State.put vs
    case Aeson.fromJSON v of
        Aeson.Success x -> return x
        Aeson.Error x -> Err.throwError $ CommunicationError x

runSplice :: Options -> Splice a -> IO (Either Error a)
runSplice = undefined

