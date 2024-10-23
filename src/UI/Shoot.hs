{-# LANGUAGE RecordWildCards #-}

module UI.Shoot (takeScreenshoot, draw) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Foreign as F
import qualified Foreign.C as F
import SDL
import qualified SDL.Internal.Types as SINT
import qualified SDL.Raw.Enum as SENUM
import qualified SDL.Raw.Video as SRAW
import UI.State (State (..))

takeScreenshoot :: FilePath -> Surface -> State -> IO ()
takeScreenshoot filename texSurface state = do
  let (width, height) = (stateZoomWidth state, stateZoomHeight state)
  putStrLn "Screenshooting...."
  withRGBSurface (V2 width height) ARGB8888 $ \surface ->
    withSRenderer surface $ \renderer -> do
      tmpTexture <- createTextureFromSurface renderer texSurface
      draw renderer tmpTexture state
      pixels <- surfacePixels surface
      void $ SRAW.renderReadPixels (r renderer) F.nullPtr SENUM.SDL_PIXELFORMAT_ARGB8888 pixels (width * 4)
      F.withCString filename $ \cFilename ->
        void $ SRAW.saveBMP (s surface) cFilename
  putStrLn "Done!"
  where
    r (SINT.Renderer rr) = rr
    s (Surface ss _) = ss

draw :: Renderer -> Texture -> State -> IO ()
draw renderer texture State {..} =
  copyEx
    renderer
    texture
    ( Just
        ( Rectangle
            (P (V2 stateOffsetX stateOffsetY))
            (V2 stateZoomWidth stateZoomHeight)
        )
    )
    Nothing
    stateRotation
    Nothing
    (V2 stateHFlip stateVFlip)

withRGBSurface :: V2 F.CInt -> PixelFormat -> (Surface -> IO ()) -> IO ()
withRGBSurface dimensions format =
  bracket (createRGBSurface dimensions format) freeSurface

withSRenderer :: Surface -> (Renderer -> IO ()) -> IO ()
withSRenderer surface =
  bracket (createSoftwareRenderer surface) destroyRenderer
