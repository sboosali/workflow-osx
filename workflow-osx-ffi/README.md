[![Build Status](https://secure.travis-ci.org/sboosali/workflow-osx-ffi.svg)](http://travis-ci.org/sboosali/workflow-osx-ffi)
[![Hackage](https://img.shields.io/hackage/v/workflow-osx-ffi.svg)](https://hackage.haskell.org/package/workflow-osx-ffi)

# workflow-osx-ffi

Automate keyboard/mouse/clipboard/application interaction.

These bindings are low-level, with minimal dependencies (only native OSX headers, and mostly ghc boot packages). For higher-level bindings, which do depend on [workflow-types](https://hackage.haskell.org/package/workflow-osx), see [workflow-osx](https://hackage.haskell.org/package/workflow-osx). 

## Examples

```
:: IO ()
do

 -- Reverse the current clipboard contents
 contents <- getClipboard
 setClipboard (reverse contents)
 
 -- Insert a character into the current application
 -- Unicode half-works (the char is inserted, but sometimes crashes the application)
 sendChar 'ξ'
 sendText "sendText = traverse_ sendChar"
 
 -- press "C-a"
 pressKeyChord [VK_CONTROL] VK_A
 
 -- Launch an application from the command line
 -- (idempotent, the app is focused if already running)
 openApplication "Notes" -- ".app" can be dropped
 -- if your app isn't bundled into a `.app`, you can just shell out to some (executable) filepath using normal haskell
 
 -- In the default browser, open a new tab and visit the URL.
 openUrl "http://google.com"
 
 -- Double-click, 800 "pixels" to the right and 10 "pixels" down
 -- (i.e. where the "close window" button might be)
 clickMouseAt (POINT 800 10) 2 
 
 -- Scroll the mouse-wheel by 120 "ticks"
 -- e.g. with my trackpad, "natural" scrolling disabled, "scrolls up"
 scrollMouse  1 120
 
```
