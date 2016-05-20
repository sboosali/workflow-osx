{-# LANGUAGE NoMonomorphismRestriction, FlexibleContextsg #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
-- | some example workflows you can derive from the primitives in 'Workflow'. (see the source)
module Workflow.OSX.Example where
import Workflow.OSX.DSL
import Workflow.OSX.Execute
import Workflow.OSX.Types

import Control.Monad                 (replicateM_)


main = do
 -- attemptWorkflow testDerived
 attemptWorkflow testChrome
 -- attemptWorkflow testDSL

attemptWorkflow a = do
 putStrLn "\n"
 putStrLn $ showWorkflow a
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
 google s
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
