{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Store

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Text              as T
import           Web.Spock.Safe

type Spock = SpockT (ReaderT FileStore IO)

main :: IO ()
main =
  do store <- getStore "jmt_store"
     runSpock 3000 $ spockT (\m -> runReaderT m store) website

website :: Spock ()
website = do get root $ do r <- lift raws
                           (text . T.pack . show) r
             get ("hello" <//> var) $ \name ->
                 text ("Hello " <> name <> "!")
