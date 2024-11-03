{-# LANGUAGE OverloadedStrings #-}

module Storage (savePath) where

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified System.Directory as Dir
import System.FilePath ((</>))

programName :: FilePath
programName = "horus"

savePath :: IO FilePath
savePath = do
  timestamp <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
  dir <- Dir.getXdgDirectory Dir.XdgData programName
  Dir.createDirectoryIfMissing True dir
  return $ dir </> iso8601Show timestamp <> ".png"
