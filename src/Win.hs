{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Win (loadScreenshoot, RawImage (..)) where

import Control.Exception.Safe (bracket)
import Foreign (Ptr, mallocBytes)
import qualified Foreign as F
import qualified Foreign.C.Types as F
import GHC.Base (divInt)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (WindowAttributes (wa_height, wa_width), getWindowAttributes)
import qualified Graphics.X11.Xlib.Types as X

data RawImage a
  = RawImage
  { rawImagePtr :: Ptr a,
    rawImageBitsPerPixel :: Int,
    rawImageBytesPerLine :: Int,
    rawImageSizeInBytes :: Int,
    rawImageWidth :: Int,
    rawImageHeight :: Int
  }
  deriving (Show)

-- TODO: free/with
loadScreenshoot :: IO (RawImage a)
loadScreenshoot = do
  withDisplay $ \display ->
    withRootImage display $ \rootImage -> do
      bps <- int <$> getBitsPerPixel rootImage
      bpl <- int <$> getBytesPerLine rootImage
      (w, h) <- getRootDimensions display
      let sizeInBytes = w * h * divInt bps 8
      rawImage <- mallocBytes sizeInBytes
      rootImageData <- wut rootImage
      F.copyBytes rawImage rootImageData sizeInBytes
      return
        RawImage
          { rawImagePtr = rawImage,
            rawImageBitsPerPixel = bps,
            rawImageBytesPerLine = bpl,
            rawImageSizeInBytes = sizeInBytes,
            rawImageWidth = w,
            rawImageHeight = h
          }
  where
    getBytesPerLine (X.Image p) = F.peekByteOff @F.CInt (F.castPtr p) (4 * szCInt + szPtr + 5 * szCInt)
    getBitsPerPixel (X.Image p) = F.peekByteOff @F.CInt (F.castPtr p) (4 * szCInt + szPtr + 6 * szCInt)
    wut (X.Image p) = F.castPtr <$> F.peek (F.plusPtr @Image @(F.Ptr F.CIntPtr) p (4 * szCInt))
    int i = fromIntegral i :: Int
    szCInt = F.sizeOf @F.CInt undefined
    szPtr = F.sizeOf @F.CIntPtr undefined

withRootImage :: Display -> (Image -> IO r) -> IO r
withRootImage display f = do
  (w, h) <- getRootDimensions display
  bracket (getImage display rootWin 0 0 (uint w) (uint h) (long allPlanes_aux) zPixmap) destroyImage f
  where
    uint i = fromIntegral i :: F.CUInt
    long i = fromIntegral i :: F.CULong
    rootWin = defaultRootWindow display

withDisplay :: (Display -> IO r) -> IO r
withDisplay = bracket (openDisplay "") closeDisplay

getRootDimensions :: Display -> IO (Int, Int)
getRootDimensions display =
  getWindowAttributes display (defaultRootWindow display) >>= \attrWin ->
    return (width attrWin, height attrWin)
  where
    int i = fromIntegral i :: Int
    height a = int $ wa_height a
    width a = int $ wa_width a
