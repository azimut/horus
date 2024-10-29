# horus

👷**WORK IN PROGRESS**👷

Small zoom and screenshot tool. Based on SDL and X11. Keyboard-centric.

## Controls

| key | mapping                 |
|-----|-------------------------|
| h   | left                    |
| j   | down                    |
| k   | up                      |
| l   | right                   |
| r   | reset                   |
| f   | flip horizontally       |
| F   | flip vertically         |
| +   | zoom in                 |
| -   | zoom out                |
| <   | rotate left             |
| >   | rotate right            |
| s   | screenshoot + clipboard |

## Installation

### From Source

``` console
$ sudo apt install -y libsdl2-dev libsdl2-image-dev libcairo2-dev libx11-dev libxext-dev libxinerama-dev libxrandr-dev libxss-dev
$ cabal build
```

## Development References
- [C code example using x11+cairo](https://chromium.googlesource.com/chromiumos/platform/window_manager/+/a2bc12e3761d2c1d41b69c2a334af60262e7bd4c/screenshot.cc)
- [Haskell code to screenshoot a ppm](https://github.com/xmonad/X11/blob/master/examples/ScreenCapture.hs)
- [Haskell code sdl2+cairo](https://github.com/haskell-gi/haskell-gi/blob/master/cairo/examples/sdl/CairoSDL.hs)
- [Haskell casting of Image](https://github.com/ndzik/horture/blob/2e631613e97ef1fb305de061c4e04ca3c8fced4c/src/Horture/Backend/X11/X11.hs#L37)
- ["SDL2 doesn't really support rendering surfaces directly anymore."](https://gamedev.stackexchange.com/questions/136055/why-doesnt-sdl-surface-rendering-work-in-sdl2)
- [X11's XImage C struct](https://tronche.com/gui/x/xlib/graphics/images.html#XImage)
- Clipboard
  - [X11: How does "the" clipboard work?](https://www.uninformativ.de/blog/postings/2017-04-02/0/POSTING-en.html)
  - [Managing the X11 Clipboard](https://jameshunt.us/writings/x11-clipboard-management-foibles/)
- Movement based on rotation
  - ["Movement and Angle" SDL's forum](https://gamedev.net/forums/topic/394008-csdl-movement-and-the-angle/3611930/)
  - ["Rotation Matrix" wiki](https://en.wikipedia.org/wiki/Rotation_matrix)
