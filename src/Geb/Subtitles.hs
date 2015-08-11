{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Geb.Subtitles where

import ClassyPrelude

import Text.Subtitles.SRT
import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.Either.Combinators


parseSubtitles :: LByteString -> [Text]
parseSubtitles bytes = map T.toStrict . cleanSubtitles $ fromRight (parseScript text) (readSRT text)
  where text = fromRight (decodeLatin1 bytes) (decodeUtf8' bytes)
        readSRT = fmap (map T.fromStrict . map dialog) . eitherResult . parse parseSRT
        parseScript = filter (not . T.null) . map (T.strip . T.intercalate ":" . drop 1 . T.splitOn ":") . T.lines


cleanSubtitles :: [LText] -> [LText]
cleanSubtitles = map cleanup . filter notGarbage
  where notGarbage l = not ("--" `T.isPrefixOf` l || "<i>" `T.isPrefixOf` l)
        cleanup = T.replace "\n" " "
