{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Lit.Report
    ( Report
    , Options (..)
    , OutputOptions (..)
    , runReport
    , reserveOutputPath
    , saveOutputFile
    , Config
    , getC
    , get
    , setC
    , ($=)
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
import           Data.Lens.Common                 (Lens, getL, lens, mapLens,
                                                   setL)
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
    { extensions :: Map.Map TypeRep Dynamic
    , options    :: Options
    -- | a counter for each file prefix in the output folder.
    , counters   :: Map.Map String Int
    }

lCounters :: Lens ReportState (Map.Map String Int)
lCounters = lens counters $ \x y -> y{counters=x}

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
    st <- getReportState
    let ok = (`elem` ['a'..'z']) . toLower
    for_ [pre, ext] $ \s ->
        unless (all ok s && not (null s)) $
            fail "saveOutputFile: invalid prefix or extension requested"
    let l     = mapLens pre . lCounters
        c     = fromMaybe 1 $ getL l st
        name  = pre ++ show c ++ "." ++ ext
        path' = path ops </> name
        url'  = url ops ++ "/" ++ name
    alreadyThere <- internalLiftIO $ doesExist path'
    -- TODO: There is a race condition here before the file is
    --       created down the line, but it is hard to avoid.
    when alreadyThere $ fail $ "File already exists: " ++ path'
    Report $ State.put $ setL l (Just $ c+1) st
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

class (Typeable a, Default a) => Config a

getC :: Config a => Report a
getC = do
    ext <- extensions <$> getReportState
    let c = Map.lookup (typeOf r) ext
        r = fromMaybe def (fromDynamic =<< c)
    return r -- Here, the type of r is fixed to a.

get :: Config a => Lens a b -> Report b
get l = getL l <$> getC

setC :: Config a => a -> Report ()
setC x = do
    state <- getReportState
    let k = typeOf x
        v = toDyn x
        ext = extensions state
        ext' = Map.insert k v ext
    Report $ State.put state{extensions = ext'}

infixr 4 $=
($=) :: Config a => Lens a b -> b -> Report ()
l $= x = x `seq` setC . setL l x =<< getC
