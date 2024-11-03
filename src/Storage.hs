{-# LANGUAGE OverloadedStrings #-}

module Storage (savePath) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified System.Directory as Dir
import System.FilePath ((</>))

programName :: FilePath
programName = "horus"

savePath :: IO FilePath
savePath = do
  timestamp <- formatTime defaultTimeLocale "%y-%m-%d %H:%M:%S" <$> getCurrentTime
  dir <- Dir.getXdgDirectory Dir.XdgData programName
  Dir.createDirectoryIfMissing True dir
  return $ dir </> timestamp <> ".png"
