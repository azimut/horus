module UI.Shoot (takeScreenshoot, surfaceFromPointer) where

import Control.Exception.Safe (bracket)
import Control.Monad (void)
import qualified Foreign as F
import qualified Foreign.C as F
import qualified Graphics.Rendering.Cairo as C
import SDL
import qualified SDL.Internal.Types as SINT
import qualified SDL.Raw.Enum as SENUM
import qualified SDL.Raw.Video as SRAW
import System.Process (spawnProcess)
import UI.Draw (draw)
import UI.State (State (..))

surfaceFromPointer :: F.Ptr a -> Int -> Int -> IO Surface
surfaceFromPointer x width height = do
  surface <- createRGBSurface (V2 (cint width) (cint height)) ARGB8888
  pixels <- surfacePixels surface
  F.copyBytes pixels (F.castPtr x) (width * height * 4)
  return surface
  where
    cint i = fromIntegral i :: F.CInt

takeScreenshoot :: FilePath -> Surface -> State -> IO ()
takeScreenshoot filename texSurface state = do
  let (width, height) = (stateZoomWidth state, stateZoomHeight state)
  withRGBSurface (V2 width height) ARGB8888 $ \surface ->
    withSRenderer surface $ \renderer -> do
      tmpTexture <- createTextureFromSurface renderer texSurface
      draw renderer tmpTexture state
      pixels <- surfacePixels surface
      void $ SRAW.renderReadPixels (r renderer) F.nullPtr SENUM.SDL_PIXELFORMAT_ARGB8888 pixels (width * 4)
      C.withImageSurfaceForData (F.castPtr pixels) C.FormatRGB24 (int width) (int height) (int (width * 4)) $ \iSurface -> do
        C.surfaceWriteToPNG iSurface filename
        void $ spawnProcess "/bin/xclip" ["-selection", "clipboard", "-t", "image/png", "-i", filename]
  where
    r (SINT.Renderer rr) = rr
    int i = fromIntegral i :: Int

withRGBSurface :: V2 F.CInt -> PixelFormat -> (Surface -> IO ()) -> IO ()
withRGBSurface dimensions format =
  bracket (createRGBSurface dimensions format) freeSurface

withSRenderer :: Surface -> (Renderer -> IO ()) -> IO ()
withSRenderer surface =
  bracket (createSoftwareRenderer surface) destroyRenderer
