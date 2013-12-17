{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Lit.Report ( 
    -- * The `Report` monad
    Report
    -- ** Extendable configuration
    , ConfigVar
    , fromTag
    , fromGetSet
    , singleton
    , refine
    , get
    , set
    , ($=)
    -- ** Limited IO in the `Report` monad
    , reserveOutputPath
    , saveOutputFile
    -- ** Running a `Report`
    , Options (..)
    , OutputOptions (..)
    , runReport
    ) where

import           Control.Applicative
import           Control.Category
import           Control.Monad                    (unless, when)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy             as B
import           Data.Char                        (toLower)
import           Data.Default
import           Data.Dynamic                     (Dynamic, fromDynamic, toDyn)
import           Data.Foldable                    (for_)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Typeable                    (TypeRep, Typeable, typeOf)
import           GHC.Generics                     (Generic)
import           Prelude                          hiding ((.))
import           System.Directory                 (doesDirectoryExist,
                                                   doesFileExist)
import           System.FilePath                  ((</>))
import qualified System.IO                        as IO

data OutputOptions = OutputOptions
    { path :: FilePath
    , url  :: String
    }
  deriving (Show, Generic)

data Options = Options
    { outputOptions :: Maybe OutputOptions
    }
  deriving (Show, Generic)

instance FromJSON OutputOptions
instance ToJSON OutputOptions
instance FromJSON Options
instance ToJSON Options

data ReportState = ReportState
    { extensions :: Map.Map (TypeRep, TypeRep) Dynamic
    , options    :: Options
    -- | a counter for each file prefix in the output folder.
    , counters   :: Map.Map String Int
    }

newtype Report a = Report (StateT ReportState IO a)
  deriving (Monad, Functor, Applicative, MonadIO)

-- | For now, Report is an instance of MonadIO, but the idea is
--   to restrict this at some point (for instance by having liftIO
--   return `fail` unless some flag is given to hlit). The below
--   function circumvents this. It should not be exported.
internalLiftIO :: IO a -> Report a
internalLiftIO = Report . liftIO

runReport :: Options -> Report a -> IO a
runReport opts (Report a) = State.evalStateT a (ReportState def opts def)

getReportState :: Report ReportState
getReportState = Report State.get

maybeOutputOptions :: Report (Maybe OutputOptions)
maybeOutputOptions = outputOptions . options <$> getReportState

requireOutputOptions :: Report OutputOptions
requireOutputOptions = maybeOutputOptions >>= \x -> case x of
    Just o -> return o
    Nothing -> fail "No ouput folder configured"

doesExist :: FilePath -> IO Bool
doesExist f = (||) <$> doesFileExist f <*> doesDirectoryExist f

reserveOutputPath
    :: String -- ^ Prefix
    -> String -- ^ Extension
    -> Report (FilePath, String) -- ^ (path, url)
reserveOutputPath pre ext = do
    ops <- requireOutputOptions
    state <- getReportState
    let ok = (`elem` ['a'..'z']) . toLower
    for_ [pre, ext] $ \s ->
        unless (all ok s && not (null s)) $
            fail "saveOutputFile: invalid prefix or extension requested"
    let counts = counters state
        count = fromMaybe 1 $ Map.lookup pre counts
        count' = count + 1
        name  = pre ++ show count ++ "." ++ ext
        path' = path ops </> name
        url'  = url ops ++ "/" ++ name
        state' = state{counters=Map.insert pre count' counts}
    alreadyThere <- internalLiftIO $ doesExist path'
    -- TODO: There is a race condition here before the file is
    --       created down the line, but it is hard to avoid.
    when alreadyThere $ fail $ "File already exists: " ++ path'
    Report $ State.put state'
    return (path', url')

-- | Save a file in the ouput folder.
saveOutputFile
    :: String        -- ^ Prefix
    -> String        -- ^ Extension
    -> ByteString    -- ^ Content to save
    -> Report String -- ^ Url to file
saveOutputFile pre ext content = do
    (path', url') <- reserveOutputPath pre ext
    internalLiftIO $
        IO.withBinaryFile path' IO.WriteMode $ \h -> B.hPut h content
    return url'

data ConfigVar a 
    = GetSetVar (Report a) (a -> Report ())

fromGetSet :: (Report a) -> (a -> Report ()) -> ConfigVar a
fromGetSet = GetSetVar

fromTag :: (Typeable a, Typeable tag) => tag -> a -> ConfigVar a
fromTag tag d = GetSetVar getter setter
  where
    t = (typeOf tag, typeOf d)
    getter = do
        ext <- extensions <$> getReportState
        let c = Map.lookup t ext
        return $ fromMaybe d (fromDynamic =<< c)
    setter x = do
        state <- getReportState
        let v = toDyn x
            ext = extensions state
            ext' = Map.insert t v ext
        Report $ State.put state{extensions = ext'}

singleton :: (Typeable a, Default a) => ConfigVar a
singleton = fromTag d d where d = def

refine :: ConfigVar a -> (a -> b) -> (b -> a -> a) -> ConfigVar b
refine v g s = fromGetSet getter setter
  where
    getter = g <$> get v
    setter n = do
        x <- get v
        v $= s n x

get :: ConfigVar a -> Report a
get (GetSetVar getter _) = getter

set :: ConfigVar a -> a -> Report ()
set (GetSetVar _ setter) = setter

infixr 4 $=
($=) :: ConfigVar a -> a -> Report ()
v $= x = x `seq` set v x
