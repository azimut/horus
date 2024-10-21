{-# LANGUAGE RecordWildCards #-}

module Store (savePng, saveBmp) where

import Codec.BMP as BMP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, runManaged)
import qualified Data.ByteString.Unsafe as BS
import qualified Foreign as F
import qualified Graphics.Rendering.Cairo as C
import Win

saveBmp :: RawImage a -> FilePath -> IO ()
saveBmp RawImage {..} filename = do
  bytes <- BS.unsafePackMallocCStringLen (F.castPtr rawImagePtr, rawImageSizeInBytes)
  BMP.writeBMP filename $
    BMP.packRGBA32ToBMP32 rawImageWidth rawImageHeight bytes

savePng :: RawImage a -> FilePath -> IO ()
savePng RawImage {..} filename = runManaged $ do
  surface <-
    managed
      ( C.withImageSurfaceForData
          (F.castPtr rawImagePtr)
          C.FormatARGB32
          rawImageWidth
          rawImageHeight
          rawImageBytesPerLine
      )
  liftIO $
    C.surfaceWriteToPNG surface filename
