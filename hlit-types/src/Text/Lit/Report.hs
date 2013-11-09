{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Lit.Report
    ( Report
    , Options (..)
    , OutputOptions (..)
    , runReport
    , mkImageFile
    , Config
    , getC
    , get
    , setC
    , ($=)
    ) where

import           Control.Applicative
import           Control.Category
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Default
import           Data.Dynamic                     (Dynamic, fromDynamic, toDyn)
import           Data.Lens.Common                 (Lens, getL, setL)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Traversable                 (forM)
import           Data.Typeable                    (TypeRep, Typeable, typeOf)
import           GHC.Generics                     (Generic)
import           Prelude                          hiding ((.))
import           System.FilePath                  ((</>))

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
    }

newtype Report a = Report (StateT ReportState IO a)
  deriving (Monad, Functor, Applicative, MonadIO)

runReport :: Options -> Report a -> IO a
runReport opts (Report a) = State.evalStateT a (ReportState def opts)

newtype ImageFileCounter = ImageFileCounter Int
  deriving (Typeable, Default)

instance Config ImageFileCounter

-- | @mkImageFile ext@ will create a tuple @(path, url)@ to
--   an image file in the output folder. E.g.
--   >>> renderIm = do
--   >>>   Just (path, url) <- mkImageFile "png"
--   >>>   liftIO $ writeImage path -- this is an action creating an image at 'path'.
--   >>>   return $ image url "" (text "some image") -- image and text from pandoc-types
mkImageFile :: String -> Report (Maybe (FilePath, String))
mkImageFile ext = do
    mop <- outputOptions . options <$> Report State.get
    forM mop $ \op -> do
        ImageFileCounter c <- getC
        setC $ ImageFileCounter $ c + 1
        let name = "im" ++ show c ++ "." ++ ext
        return (path op </> name, url op ++ name)

class (Typeable a, Default a) => Config a

getC :: Config a => Report a
getC = do
    ext <- extensions <$> Report State.get
    let c = Map.lookup (typeOf r) ext
        r = fromMaybe def (fromDynamic =<< c)
    return r -- Here, the type of r is fixed to a.

get :: Config a => Lens a b -> Report b
get l = getL l <$> getC

setC :: Config a => a -> Report ()
setC x = do
    state <- Report State.get
    let k = typeOf x
        v = toDyn x
        ext = extensions state
        ext' = Map.insert k v ext
    Report $ State.put state{extensions = ext'}

infixr 4 $=
($=) :: Config a => Lens a b -> b -> Report ()
l $= x = x `seq` setC . setL l x =<< getC
