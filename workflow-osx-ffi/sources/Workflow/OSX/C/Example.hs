
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
-- | some example workflows you can derive from the primitives in 'Workflow'. (see the source)
module Workflow.OSX.C.Example where
import qualified Workflow.OSX.C.Bindings as Cocoa
import Workflow.OSX.C

import Control.Monad                 (replicateM_)

--------------------------------------------------------------------------------

{- |

@
stack build && stack exec -- example-workflow-osx
@

OSX.sendText_byChar
OSX.sendText_byKey
OSX.sendText_byClipboard
OSX.sendKeyChord_flags
OSX.sendMouseClick
OSX.getClipboard
OSX.setClipboard
OSX.currentApplication
OSX.openApplication
OSX.openURL
TODO: OSX.sendMouseScroll

-}
main = do
 putStrLn "Workflow.OSX.Example..."
 delayMilliseconds 1000

 -- attemptWorkflow testInsert -- Works
 -- attemptWorkflow testDerived -- Doesn't work

 -- attemptworkflow testHolding
 -- attemptWorkflow testChrome
 -- attemptWorkflow testDSL
 -- testMouse

 attemptScreenshot

--------------------------------------------------------------------------------

attemptWorkflow :: WorkflowT IO a -> IO a
attemptWorkflow a = do
 putStrLn "\n"
 --TODO putStrLn $ showWorkflow a
 runWorkflowT
   defaultOSXWorkflowConfig{osxHowToSendText = SendTextByChar, osxStepDelay = 30}
   a

cut :: (MonadWorkflow m) => m String
cut = do
 sendKeyChord [CommandModifier] XKey
 delay 100 -- TODO how long does it need to wait?
 getClipboard

-- | access the currently selected region from Haskell, via the clipboard
copy :: (MonadWorkflow m) => m String
copy = do
 sendKeyChord [CommandModifier] CKey
 delay 100 -- TODO how long does it need to wait?
 getClipboard

paste :: (MonadWorkflow m) => m ()
paste = do
 sendKeyChord [CommandModifier] VKey

--------------------------------------------------------------------------------

testMouse = do
 -- Cocoa.clickMouse 0 2 OSXLeftButton
 -- delayMilliseconds 30
 -- Cocoa.clickMouseAt 0 1 OSXLeftButton (CGPoint 0 0)

 -- other *is* middle button.
 -- middle-click in terminal pastes the most recently selected text.
 delayMilliseconds 1000
 Cocoa.clickMouse 0 1 OSXMiddleButton

testInsert = do
  insert "aα"

-- it worked once! and then it didn't.
testDerived = do
 s <- cut
 delay 30
 --insert "w"
 insert $ reverse s

testDSL :: Workflow ClipboardText
testDSL = do

 -- delay 30
 sendKeyChord [CommandModifier, ShiftModifier] BKey
 delay 1000
 sendKeyChord [CommandModifier] DownArrowKey

 app <- currentApplication
 s <- getClipboard
 setClipboard app
 getClipboard

markWord = do
 sendKeyChord [OptionModifier] LeftArrowKey
 sendKeyChord [OptionModifier, ShiftModifier] RightArrowKey

backWord = do
 sendKeyChord [OptionModifier] LeftArrowKey

forWord = do
 sendKeyChord [OptionModifier] RightArrowKey

-- keyboard shortcuts don't need lag between each Keypress (hence
-- 'replicateM_', without 'interleave $ delay 25000'). only
-- interworkflow needs lag (e.g. a mini-buffer pop-up).
-- tested in Chrome.
-- testChrome :: Workflow ()
testChrome = do
 delay 5000
 replicateM_ 10 forWord
 delay 1000
 replicateM_ 10 backWord
 delay 1000
 markWord

--------------------------------------------------------------------------------

whichDynamicImage
  :: PNG.DynamicImage
  -> String
whichDynamicImage = \case
 PNG.ImageY8 _ -> "ImageY8"
 PNG.ImageY16 _ -> "ImageY16"
 PNG.ImageYF _ -> "ImageYF"
 PNG.ImageYA8 _ -> "ImageYA8"
 PNG.ImageYA16 _ -> "ImageYA16"
 PNG.ImageRGB8 _ -> "ImageRGB8"
 PNG.ImageRGB16 _ -> "ImageRGB16"
 PNG.ImageRGBF _ -> "ImageRGBF"
 PNG.ImageRGBA8 _ -> "ImageRGBA8"
 PNG.ImageRGBA16 _ -> "ImageRGBA16"
 PNG.ImageYCbCr8 _ -> "ImageYCbCr8"
 PNG.ImageCMYK8 _ -> "ImageCMYK8"
 PNG.ImageCMYK16 _ -> "ImageCMYK16"

fromDynamicImage
  :: (forall pixel. PNG.Image pixel -> r)
  -> PNG.DynamicImage
  -> r
fromDynamicImage f = \case
 PNG.ImageY8 i -> f i
 PNG.ImageY16 i -> f i
 PNG.ImageYF i -> f i
 PNG.ImageYA8 i -> f i
 PNG.ImageYA16 i -> f i
 PNG.ImageRGB8 i -> f i
 PNG.ImageRGB16 i -> f i
 PNG.ImageRGBF i -> f i
 PNG.ImageRGBA8 i -> f i
 PNG.ImageRGBA16 i -> f i
 PNG.ImageYCbCr8 i -> f i
 PNG.ImageCMYK8 i -> f i
 PNG.ImageCMYK16 i -> f i

brightenRGB8 :: Int -> PNG.Image PNG.PixelRGB8 -> PNG.Image PNG.PixelRGB8
brightenRGB8 i = PNG.pixelMap go
  where
  up v = fromIntegral (fromIntegral v + i)
  go (PNG.PixelRGB8 r g b) = PNG.PixelRGB8 (up r) (up g) (up b)

{-

e.g.

@
@

-}
attemptScreenshot = do
 takeScreenshot >>= \case
   Left e -> putStrLn e
   Right i -> do
     print $ whichDynamicImage i
     print $ fromDynamicImage getDimensions i
     let a = PNG.convertRGB8 i
     let b = brightenRGB8 100 a
     PNG.savePngImage "brighter_screenshot.png" (PNG.ImageRGB8 b)
     return ()
 where
 getDimensions :: PNG.Image x -> (Int,Int)
 getDimensions PNG.Image{..} = (imageWidth,imageHeight)

--------------------------------------------------------------------------------
