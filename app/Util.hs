module Util (getXdgDataDir) where

import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)

getXdgDataDir :: IO FilePath
getXdgDataDir = do
  dir <- getXdgDirectory XdgData "monomer-flatpak-example"
  createDirectoryIfMissing True dir
  pure dir
