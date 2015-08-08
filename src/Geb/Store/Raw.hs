module Geb.Store.Raw where

import           Geb.Utils

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.FileStore
import           System.FilePath

data Raw = Raw
  { rawName    :: String
  , rawContent :: ByteString
  } deriving Show

addRaw :: FileStore -> Raw -> IO ()
addRaw store raw  = create store path defaultAuthor description (rawContent raw)
  where path = "raw" </> rawName raw
        description = "Added file " ++ rawName raw

getRaw :: FileStore -> FilePath -> IO Raw
getRaw store f = do content <- retrieve store ("raw" </> f) Nothing
                    return (Raw f content)

raws :: FileStore -> IO [Raw]
raws store = do allInRaw <- catchConst NotFound
                                       (directory store "raw")
                                       (return [])
                let rawFiles = map toFilePath allInRaw
                forM rawFiles (getRaw store)
  where toFilePath (FSFile f) = f
        toFilePath (FSDirectory f) = error ("Unexpected directory in raw: " ++ f)

defaultAuthor :: Author
defaultAuthor = Author "Geb system" "geb@compiletoi.net"
