{-# LANGUAGE ViewPatterns #-}
{-| low-level bindings, with Haskell types.

-}
module Workflow.OSX.Bindings where
import Workflow.OSX.Extra
import Workflow.OSX.Foreign
import Workflow.OSX.Marshall
import Workflow.OSX.Types

import Foreign.C.String                   (peekCString, withCString)
import Data.Char (ord)
import Control.Monad.IO.Class
import Numeric.Natural


sendText :: (MonadIO m) => String -> m ()
-- sendText :: (MonadIO m) => (Monad m  m) => String -> m ()
sendText s = liftIO $
 sequence_ $ intersperse (delayMilliseconds 30) (fmap sendChar s)
 --NOTE must pause between events

{-| Haskell chars are Unicode code-points:

>>> import Data.Char (ord)
>>> map ord "abc"
[97,98,99]
>>> map ord "αβγ"
[945,946,947]
>>> [0x03B1,0x03B2,0x03B3]
[945,946,947]

TODO there are 1,000,000 hs chars, but UniChar only holds 2^16 ~ 65,000. utf-8 encoding?

-}
sendChar :: (MonadIO m) => Char -> m ()
sendChar c = liftIO $
 c_sendChar (unsafeIntToWord16 . ord $ c) --TODO
-- sendChar :: (MonadIO m) => Char -> m ()
-- sendChar c = liftIO $ c_sendChar (ord c)

-- |
pressKey :: (MonadIO m) => [Modifier] -> Key -> m ()
pressKey (marshallModifiers -> flags) (marshallKey -> key) = liftIO $
 c_pressKey flags key

-- | e.g. @clickMouse 0 1 (CGMouseButtonLeft,NX_LMOUSEDOWN,NX_LMOUSEUP)@
clickMouse
 :: (MonadIO m)
 => CGEventFlags
 -> Natural
 -> (CGMouseButton,CGEventType,CGEventType) -- ^ must be consistent
 -> m ()
clickMouse modifiers (unsafeNatToWord32 -> times) (button,downEvent,upEvent)
 = liftIO $ do
    c_clickMouse modifiers times button downEvent upEvent

--old
  --  with $ \p ->
  --     c_clickMouseAt modifiers button downEvent upEvent times

-- TODO |
-- clickMouse :: (MonadIO m) => [Modifier] -> Positive -> MouseButton -> m ()
-- clickMouse (MouseClick (marshallModifiers -> flags) (marshallPositive -> n) (marshallButton -> button)) = c_clickMouse

-- |
getClipboard :: (MonadIO m) => m ClipboardText
getClipboard = liftIO $
 c_getClipboard >>= peekCString

-- |
--
-- note: unlike the keyboard shortcuts of 'copy',
-- contents don't show up in Alfred's clipboard history.
setClipboard :: (MonadIO m) => ClipboardText -> m ()
setClipboard s = liftIO $
 withCString s c_setClipboard

currentApplication :: (MonadIO m) => m Application
currentApplication = liftIO $ do
 path <- currentApplicationPath
 return path

-- |
-- TODO Applications whose name/paths have Unicode characters may or may not marshall correctly. they should, unless we need CWString.
currentApplicationPath :: (MonadIO m) => m String
currentApplicationPath = liftIO $
 c_currentApplicationPath >>= peekCString

-- |
openURL :: (MonadIO m) => URL -> m ()
openURL s = liftIO $
 withCString s c_openURL

-- |
openApplication :: (MonadIO m) => Application -> m ()
openApplication s = liftIO $
 withCString s c_openApplication

-- holdKeyFor :: (MonadIO m) => Int -> [Modifier] -> Key -> m ()
-- holdKeyFor milliseconds (marshallModifiers -> flags) (marshallKey -> key) = liftIO $
--  c_pressKeyDown flags key
