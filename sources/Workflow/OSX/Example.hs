{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
-- | some example workflows you can derive from the primitives in 'Workflow'. (see the source)
module Workflow.OSX.Example where
import qualified Workflow.OSX.Bindings as Cocoa
import Workflow.OSX

import Workflow.Core

import Control.Monad                 (replicateM_)

{- |

@
stack build && stack exec -- example-workflow-osx
@

-}
main = do
  delayMilliseconds 1000

  attemptWorkflow testHolding
 -- attemptWorkflow testDerived
 -- attemptWorkflow testChrome
 -- attemptWorkflow testDSL
 -- attemptWorkflow testAll
 -- Cocoa.sendText "aα"
 -- testMouse

testHolding = do
  -- press "H-t"
  s <- copy
  insert "w"
  -- insert s

testMouse = do
 -- Cocoa.clickMouse 0 2 OSXLeftButton
 -- delayMilliseconds 30
 -- Cocoa.clickMouseAt 0 1 OSXLeftButton (CGPoint 0 0)

 -- other *is* middle button.
 -- middle-click in terminal pastes the most recently selected text.
 delayMilliseconds 1000
 Cocoa.clickMouse 0 1 OSXMiddleButton

testAll = do
  insert "aα"

attemptWorkflow a = do
 putStrLn "\n"
 -- putStrLn $ showWorkflow a
 runWorkflow a

testDerived = do
 _ <- copy
 delay 100
 paste

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
testChrome :: Workflow ()
testChrome = do
 delay 5000
 replicateM_ 10 forWord
 delay 1000
 replicateM_ 10 backWord
 delay 1000
 markWord

-- | access the currently selected region from Haskell, via the clipboard
copy :: (MonadWorkflow m) => m String
copy = do
 sendKeyChord [CommandModifier] CKey
 delay 100 -- TODO how long does it need to wait?
 getClipboard

paste :: (MonadWorkflow m) => m ()
paste = do
 sendKeyChord [CommandModifier] VKey
