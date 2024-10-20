{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (managed, runManaged)
import Foreign.C (CInt, CUInt (CUInt), CULong (CULong))
import qualified Graphics.Rendering.Cairo as C
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (WindowAttributes (wa_height, wa_width), getWindowAttributes)

main :: IO ()
main = runManaged $ do
  display <- managed withDisplay
  (w, h) <- liftIO $ getRootDimensions display
  image <- managed (withImage display (cuint w) (cuint h))
  surface <- managed (C.withImageSurface C.FormatRGB24 (int w) (int h))
  liftIO $ do
    C.surfaceWriteToPNG surface "/home/sendai/test.png"
    putStrLn "foo"
  where
    cuint i = fromIntegral i :: CUInt
    int i = fromIntegral i :: Int

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
