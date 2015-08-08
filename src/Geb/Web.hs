{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Geb.Web where

import           Geb.Store
import           Geb.Store.Raw
import Geb.Store.Script
import Geb.Utils

import Yesod
import System.FilePath
import qualified Data.FileStore as FS
import Text.Blaze.Html


data App = App
  { store :: FileStore
  }

mkYesod "App" [parseRoutes|
/             HomeR   GET
/script/#FilePath ScriptR GET
|]
instance Yesod App

getHomeR :: Handler Html
getHomeR = do App {..} <- getYesod
              allRaws <- liftIO (raws store)
              defaultLayout $ do
                      setTitle "Homepage"
                      [whamlet|
                          <ul>
                            $forall raw <- allRaws
                              <li>#{rawName raw} &mdash; 
                                <a href=@{ScriptR (rawName raw)}>script
                      |]

getScriptR :: FilePath -> Handler Html
getScriptR path = do App {..} <- getYesod
                     script <- liftIO $ catchConst FS.NotFound
                                                   (FS.retrieve store ("script" </> path) Nothing)
                                                   (convertRaw store path)
                     defaultLayout $ do
                             setTitle (toHtml ("Script for " ++ path))
                             [whamlet|
                                 <ul>
                                   $forall line <- scriptLines script
                                     <li>#{line}
                             |]

runGeb :: IO ()
runGeb =
  do store <- getStore "geb_store"
     warp 3000 App {..}
