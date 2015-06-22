{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | This module handles the script store

module Store where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.FileStore
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           System.FilePath

-- store :: FileStore
-- store = gitFileStore "geb_store" --TODO configuration

-- TODO copy the API of filestore with MonadStore ?
type MonadStore m = (MonadReader FileStore m, MonadIO m, MonadCatch m)

initializeStore :: MonadStore m => m ()
initializeStore = do store <- ask
                     catchConst RepositoryExists
                                (liftIO $ initialize store)
                                (return ())

data Raw = Raw
  { rawName    :: String
  , rawContent :: Text
  } deriving Show

addRaw :: MonadStore m => Raw -> m ()
addRaw raw  = do store <- ask
                 liftIO $ create store path defaultAuthor description (rawContent raw)
  where path = "raw" </> rawName raw
        description = "Added file " ++ rawName raw

raws :: MonadStore m => m [Raw]
raws = do store <- ask
          allInRaw <- catchConst NotFound
                                 (liftIO $ directory store "raw")
                                 (return [])
          let rawFiles = map toFilePath allInRaw
          liftIO $ forM rawFiles $ \f ->
               do content <- retrieve store ("raw" </> f) Nothing
                  return (Raw f content)

  where toFilePath (FSFile f) = f
        toFilePath (FSDirectory f) = error ("Unexpected directory in raw: " ++ f)

defaultAuthor :: Author
defaultAuthor = Author "Geb system" "geb@compiletoi.net"


instance Contents Text where
  fromByteString = T.decodeUtf8 . toStrict
  toByteString = fromStrict . T.encodeUtf8

catchConst :: (Exception e, Eq e, MonadCatch m) => e -> m a -> m a -> m a
catchConst exc f def = catchJust (guard . (== exc))
                                 f
                                 (const def)
