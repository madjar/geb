{-# LANGUAGE OverloadedStrings #-}
module Geb.Web where

import           Geb.Store

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text              as T
import           Web.Spock.Safe

type Spock = SpockT (ReaderT FileStore IO)

runGeb :: IO ()
runGeb =
  do store <- getStore "jmt_store"
     runSpock 3000 $ spockT (\m -> runReaderT m store) website

website :: Spock ()
website = do get root $ do r <- lift raws
                           (text . T.pack . show) r
             get ("hello" <//> var) $ \name ->
                 text ("Hello " <> name <> "!")
