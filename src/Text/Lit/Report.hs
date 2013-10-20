{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Lit.Report
    ( Report
    , runReport
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Typeable

data ReportState = ReportState

newtype Report a = Report {getUnderlying :: StateT ReportState IO a}
  deriving (Monad, Functor, Applicative, MonadIO, Typeable)

runReport :: Report a => IO a
runReport c = State.evalStateT (getUnderlying c) ReportState
