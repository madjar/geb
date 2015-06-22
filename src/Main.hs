{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Store

import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text              as T
import           Web.Spock.Safe

main :: IO ()
main =
  do initializeStore
     runSpock 3000 $ spockT id website

website :: SpockT IO ()
website = do get root $ do r <- liftIO raws
                           (text . T.pack . show) r
             get ("hello" <//> var) $ \name ->
                 text ("Hello " <> name <> "!")
