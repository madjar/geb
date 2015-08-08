{-# LANGUAGE TemplateHaskell #-}

module Geb.Store.Script where

import Geb.Store.Raw

import Data.Text (Text)
import Data.FileStore
import Data.Aeson
import Data.Aeson.TH
import System.FilePath
import Data.ByteString.Lazy (ByteString)

data Script = Script
  { scriptName :: FilePath
  , scriptLines :: [Text]
  } deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''Script)

instance Contents Script where
  fromByteString x = case eitherDecode x of
                       Left m -> error m
                       Right v -> v
  toByteString = encode

convertRaw :: FileStore -> FilePath -> IO Script
convertRaw store r = do raw <- getRaw store r
                        let lines = parseScript (rawContent raw)
                            script = Script r lines
                        save store ("scripts" </> r) defaultAuthor "plop" script
                        return script

parseScript :: ByteString -> [Text]
parseScript = undefined
