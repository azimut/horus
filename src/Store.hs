{-# LANGUAGE RecordWildCards #-}

module Store (save) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, runManaged)
import qualified Foreign as F
import qualified Graphics.Rendering.Cairo as C
import Win

save :: RawImage a -> FilePath -> IO ()
save RawImage {..} filename = runManaged $ do
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
