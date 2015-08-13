{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Geb.Web where

import           Geb.Store
import           Geb.Store.Raw
import Geb.Store.Script
import Geb.Utils

import Yesod
import System.FilePath
import qualified Data.FileStore as FS
import Text.Blaze.Html
import Data.Text (Text)

data App = App
  { store :: FileStore
  }

mkYesod "App" [parseRoutes|
/                 HomeR     GET
/submit/          SubmitR   GET POST
/script/#FilePath ScriptR   GET
|]
instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

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
type Form a = Html -> MForm Handler (FormResult a, Widget)
submitForm :: Form (Text, FileInfo)
submitForm = renderDivs $ (,)
    <$> areq textField "Title" Nothing
    <*> fileAFormReq "File"

getSubmitR :: Handler Html
getSubmitR = do
  ((res, widget), enctype) <- runFormPost submitForm
  case res of
    FormSuccess (title, fileInfo) -> redirect HomeR
    _ -> defaultLayout
           [whamlet|
             <form method=post action=@{SubmitR} enctype=#{enctype}>
               ^{widget}
               <input type=submit>
           |]

postSubmitR :: Handler Html
postSubmitR = getSubmitR

runGeb :: IO ()
runGeb =
  do store <- getStore "geb_store"
     warp 3000 App {..}
