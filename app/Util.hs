{-# LANGUAGE QuasiQuotes #-}

module Util
  ( getXdgDataDir,
    osPathToText,
  )
where

import Data.Text (Text, pack)
import GHC.IO.Encoding (utf8)
import System.Directory.OsPath (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.OsPath (OsPath, decodeWith, osp)

getXdgDataDir :: IO OsPath
getXdgDataDir = do
  dir <- getXdgDirectory XdgData [osp|monomer-flatpak-example|]
  createDirectoryIfMissing True dir
  pure dir

osPathToText :: OsPath -> Text
osPathToText path =
  case decodeWith utf8 utf8 path of
    Left err -> pack (show err)
    Right fp -> pack fp
