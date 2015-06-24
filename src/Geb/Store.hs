{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module handles the script store

module Geb.Store (FileStore, getStore, addRaw, raws) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.FileStore
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           System.FilePath


getStore :: FilePath -> IO FileStore
getStore f = do let store = gitFileStore f
                initializeStore store
                return store

initializeStore :: FileStore -> IO ()
initializeStore store = catchConst RepositoryExists
                                   (initialize store)
                                   (return ())

data Raw = Raw
  { rawName    :: String
  , rawContent :: Text
  } deriving Show

addRaw :: FileStore -> Raw -> IO ()
addRaw store raw  = create store path defaultAuthor description (rawContent raw)
  where path = "raw" </> rawName raw
        description = "Added file " ++ rawName raw

raws :: FileStore -> IO [Raw]
raws store = do allInRaw <- catchConst NotFound
                                       (directory store "raw")
                                       (return [])
                let rawFiles = map toFilePath allInRaw
                forM rawFiles $ \f ->
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
