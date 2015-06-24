module Geb.StoreTest where

import           Geb.Store

import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.HUnit

storeTests = testGroup "Store"
  [ testCase "getStore should create the folder" $
     withTempDir $ \tmpDir -> do
       let storePath = tmpDir </> "geb_store"
       getStore storePath
       doesDirectoryExist storePath @? "Store dir does not exist"
  ]


withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "geb_test_XXX"
