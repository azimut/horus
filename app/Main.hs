module Main where

import Control.Exception (bracket)
import Control.Monad.Managed (managed, runManaged)
import Foreign.C (CInt, CUInt)
import qualified Graphics.Rendering.Cairo as C
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (WindowAttributes (wa_height, wa_width), getWindowAttributes)

main :: IO ()
main = do
  withDisplay $ \display -> do
    (w, h) <- getRootDimensions display
    withImage display (cuint w) (cuint h) $ \image ->
      C.withImageSurface C.FormatRGB24 (int w) (int h) $ \surface ->
        putStrLn "foo"
  where
    cuint i = fromIntegral i :: CUInt
    int i = fromIntegral i :: Int

withImage :: Display -> CUInt -> CUInt -> (Image -> IO ()) -> IO ()
withImage display w h f = do
  bracket (getImage display (defaultRootWindow display) 0 0 w h (-1) xyPixmap) destroyImage f

getRootDimensions :: Display -> IO (CInt, CInt)
getRootDimensions display = getDimensions display (defaultRootWindow display)

withDisplay :: (Display -> IO ()) -> IO ()
withDisplay = bracket (openDisplay "") closeDisplay

getDimensions :: Display -> Window -> IO (CInt, CInt)
getDimensions dpy win =
  getWindowAttributes dpy win >>= \attrWin ->
    pure (wa_width attrWin, wa_height attrWin)
