{-# LANGUAGE OverloadedStrings #-}
module Geb.Web where

import           Geb.Store
import           Geb.Store.Raw

import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text              as T
import           Web.Spock.Safe


runGeb :: IO ()
runGeb =
  do store <- getStore "jmt_store"
     runSpock 3000 $ spockT id $ website store

website :: FileStore -> SpockT IO ()
website store = do get root $ do r <- liftIO (raws store)
                                 (text . T.pack . show) r
                   get ("hello" <//> var) $ \name ->
                       text ("Hello " <> name <> "!")
