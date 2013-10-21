{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Lit.Report
    ( Report
    , runReport
    , Config
    , getC
    , setC
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Default
import           Data.Dynamic                     (Dynamic, fromDynamic, toDyn)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Typeable                    (TypeRep, Typeable, typeOf)

data ReportState = ReportState
    { extensions :: Map.Map TypeRep Dynamic
    }

instance Default ReportState where
    def = ReportState
        { extensions = def
        }

newtype Report a = Report (StateT ReportState IO a)
  deriving (Monad, Functor, Applicative, MonadIO, Typeable)

runReport :: Report a -> IO a
runReport (Report a) = State.evalStateT a def

class (Typeable a, Default a) => Config a

getC :: Config a => Report a
getC = do
    ext <- extensions <$> Report State.get
    let c = Map.lookup (typeOf r) ext
        r = fromMaybe def (fromDynamic =<< c)
    return r -- Here, the type of r is fixed to a.

setC :: Config a => a -> Report ()
setC x = do
    state <- Report State.get
    let k = typeOf x
        v = toDyn x
        ext = extensions state
        ext' = Map.insert k v ext
    Report $ State.put state{extensions = ext'}
