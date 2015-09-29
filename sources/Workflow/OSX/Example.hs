{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-matches #-}
module Workflow.OSX.Example where
import Workflow.OSX.DSL
import Workflow.OSX.Execute
import Workflow.OSX.Types

import Control.Monad                 (replicateM_)
import Data.Monoid                   ((<>))


main = do
 attemptActions testDerived

attemptActions a = do
 putStrLn ""
 putStrLn $ showActions a
 runActions a

testDerived = do
 _ <- copy
 delay 100
 paste

testDSL :: Actions ClipboardText
testDSL = do

 -- delay 30
 sendKeyChord [CommandMod, Shift] BKey
 delay 1000
 sendKeyChord [CommandMod] DownArrowKey

 app <- currentApplication
 s <- getClipboard
 openURL $ "https://www.google.com/search?q=" <> s
 setClipboard app
 getClipboard

markWord = do
 sendKeyChord [Option       ] LeftArrowKey
 sendKeyChord [Option, Shift] RightArrowKey

backWord = do
 sendKeyChord [Option] LeftArrowKey

forWord = do
 sendKeyChord [Option] RightArrowKey


-- keyboard shortcuts don't need lag between each Keypress (hence
-- 'replicateM_', without 'interleave $ delay 25000'). only
-- interaction needs lag (e.g. a mini-buffer pop-up).
-- tested in Chrome.
testChrome :: Actions ()
testChrome = do
 delay 5000
 replicateM_ 10 forWord
 delay 1000
 replicateM_ 10 backWord
 delay 1000
 markWord

