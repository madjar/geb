-- | This module handles the script store

module Geb.Store (FileStore, getStore) where

import           Geb.Utils

import           Data.FileStore


-- | Return a git FileStore at the given FilePath, initializes it if necessary
getStore :: FilePath -> IO FileStore
getStore f = do let store = gitFileStore f
                initializeStore store
                return store

-- | Initialize a FileStore. If it already exists, do nothing
initializeStore :: FileStore -> IO ()
initializeStore store = catchConst RepositoryExists
                                   (initialize store)
                                   (return ())
