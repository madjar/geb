{-# LANGUAGE OverloadedStrings #-}
module Geb.Web where

import           Geb.Store
import           Geb.Store.Raw

import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text              as T
import Data.Text.Lazy (toStrict)
import           Web.Spock.Safe
import Lucid
import Control.Applicative

runGeb :: IO ()
runGeb =
  do store <- getStore "jmt_store"
     runSpock 3000 $ spockT id $ website store

website :: FileStore -> SpockT IO ()
website store = do get root $ index store
                   get ("hello" <//> var) $ \name ->
                       text ("Hello " <> name <> "!")


index :: MonadIO m => FileStore -> ActionT m b
index store = do allRaws <- liftIO (raws store)
                 render $ ul_ $ mapM (li_ . toHtml . show) allRaws


render :: MonadIO m => Html a -> ActionT m b
render = html . toStrict . renderText . doctypehtml_ . body_
