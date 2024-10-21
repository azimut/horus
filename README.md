``` haskell
createRGBSurfaceFrom     :: MonadIO m => Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m (Ptr Surface)
createTextureFromSurface :: MonadIO m => Renderer -> Ptr Surface -> m Texture
```

## References
- [C code example using x11+cairo](https://chromium.googlesource.com/chromiumos/platform/window_manager/+/a2bc12e3761d2c1d41b69c2a334af60262e7bd4c/screenshot.cc)
- [Haskell code to screenshoot a ppm](https://github.com/xmonad/X11/blob/master/examples/ScreenCapture.hs)
- [Haskell code sdl2+cairo](https://github.com/haskell-gi/haskell-gi/blob/master/cairo/examples/sdl/CairoSDL.hs)
- [Haskell casting of Image](https://github.com/ndzik/horture/blob/2e631613e97ef1fb305de061c4e04ca3c8fced4c/src/Horture/Backend/X11/X11.hs#L37)
- ["SDL2 doesn't really support rendering surfaces directly anymore."](https://gamedev.stackexchange.com/questions/136055/why-doesnt-sdl-surface-rendering-work-in-sdl2)
