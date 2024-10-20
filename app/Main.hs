{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, runManaged)
import qualified Foreign as F
import Foreign.C (CInt, CUInt (CUInt), CULong (CULong))
import qualified Foreign.C as F
import qualified Graphics.Rendering.Cairo as C
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (WindowAttributes (wa_height, wa_width), getWindowAttributes)
import Graphics.X11.Xlib.Types (Image (..))

main :: IO ()
main = runManaged $ do
  display <- managed withDisplay
  (w, h) <- liftIO $ getRootDimensions display
  image <- managed (withImage display (cuint w) (cuint h))
  -- surface <- managed (C.withImageSurface C.FormatRGB24 (int w) (int h))
  -- type PixelData = Ptr CUChar
  -- newtype Image = Image (Ptr Image)
  real <- liftIO $ wut image
  strides <- liftIO $ fromIntegral <$> www image
  surface <- managed (C.withImageSurfaceForData real C.FormatARGB32 (int w) (int h) strides)
  liftIO $ do
    C.surfaceWriteToPNG surface "/home/sendai/test.png"
    putStrLn "Done!"
  where
    cuint i = fromIntegral i :: CUInt
    int i = fromIntegral i :: Int
    www (Image p) = F.peekByteOff @F.CInt (F.castPtr p) (4 * szCInt + szPtr + 5 * szCInt)
    wut (Image p) = F.castPtr <$> F.peek (F.plusPtr @Image @(F.Ptr F.CIntPtr) p (4 * szCInt))

szCInt :: Int
szCInt = F.sizeOf @F.CInt undefined

szPtr :: Int
szPtr = F.sizeOf @F.CIntPtr undefined

szCULong :: Int
szCULong = F.sizeOf @F.CULong undefined

withImage :: Display -> CUInt -> CUInt -> (Image -> IO r) -> IO r
withImage display w h f = do
  bracket (getImage display (defaultRootWindow display) 0 0 w h allPlanes zPixmap) destroyImage f
  where
    allPlanes = fromIntegral allPlanes_aux :: CULong

getRootDimensions :: Display -> IO (CInt, CInt)
getRootDimensions display = getDimensions display (defaultRootWindow display)

withDisplay :: (Display -> IO r) -> IO r
withDisplay = bracket (openDisplay "") closeDisplay

getDimensions :: Display -> Window -> IO (CInt, CInt)
getDimensions dpy win =
  getWindowAttributes dpy win >>= \attrWin ->
    pure (wa_width attrWin, wa_height attrWin)
