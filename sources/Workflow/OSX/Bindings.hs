{-# LANGUAGE ViewPatterns #-}
{-| low-level bindings, with Haskell types.

-}
module Workflow.OSX.Bindings where
import Workflow.OSX.Extra
import Workflow.OSX.Foreign
import Workflow.OSX.Marshall
import Workflow.OSX.Types

import Foreign
import Foreign.C
import Foreign.C.String                   (peekCString, withCString)
import Data.Char (ord)
import Control.Monad.IO.Class
import Numeric.Natural
import Control.Exception (bracket,bracket_)

--------------------------------------------------------------------------------
-- Workflow types

sendText :: (MonadIO m) => String -> m ()
-- sendText :: (MonadIO m) => (Monad m  m) => String -> m ()
sendText s = liftIO $
 sequence_ $ intersperse (delayMilliseconds 30) (fmap sendChar s)
 --NOTE must pause between events

-- |
sendKeyChord_ :: (MonadIO m) => [Modifier] -> Key -> m () --TODO Only this identifier's ambiguous with wf types?
sendKeyChord_ (marshallModifiers -> flags) (marshallKey -> key) = liftIO $
  c_pressKey flags key

sendMouseClick :: (MonadIO m) => [Modifier] -> Natural -> MouseButton -> m ()
sendMouseClick (marshallModifiers -> flags) times (marshallButton -> button) =
 clickMouse flags times button

-- sendMouseScroll
-- sendMouseScroll =

pressKey :: (MonadIO m) => [Modifier] -> Key -> m ()
pressKey = sendKeyChord_ --TODO rm

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | e.g. @clickMouse 0 1 (CGMouseButtonLeft,NX_LMOUSEDOWN,NX_LMOUSEUP)@
clickMouse
 :: (MonadIO m)
 => CGEventFlags
 -> Natural
 -> (CGMouseButton,CGEventType,CGEventType) -- ^ must be consistent
 -> m ()
clickMouse modifiers times button = liftIO $ do
 p <- getCursorPosition
 clickMouseAt modifiers times button p

--old
  --  with $ \p ->
  --     c_clickMouseAt modifiers button downEvent upEvent times

-- | e.g. @clickMouse 0 1 (CGMouseButtonLeft,NX_LMOUSEDOWN,NX_LMOUSEUP)@
clickMouseAt
 :: (MonadIO m)
 => CGEventFlags
 -> Natural
 -> (CGMouseButton,CGEventType,CGEventType) -- ^ must be consistent
 -> CGPoint
 -> m ()
clickMouseAt
 modifiers
 (unsafeNatToWord32 -> times) --TODO bound
 (button,downEvent,upEvent)
 (CGPoint x y)
 = liftIO $ do
     c_clickMouseAt modifiers times button downEvent upEvent x y

-- TODO |
-- clickMouse :: (MonadIO m) => [Modifier] -> Positive -> MouseButton -> m ()
-- clickMouse (MouseClick (marshallModifiers -> flags) (marshallPositive -> n) (marshallButton -> button)) = c_clickMouse

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

{-| find the mouse.

-}
getCursorPosition :: (MonadIO m) => m CGPoint
getCursorPosition = liftIO $ do
 getByReference c_getCursorPosition

{-| move the mouse.

(doesn't trigger @RSIGuard@'s @AutoClick@).

-}
setCursorPosition :: (MonadIO m) => CGPoint -> m ()
setCursorPosition (CGPoint x y) = liftIO $ do
  c_setCursorPosition x y

--------------------------------------------------------------------------------

-- holdKeyFor :: (MonadIO m) => Int -> [Modifier] -> Key -> m ()
-- holdKeyFor milliseconds (marshallModifiers -> flags) (marshallKey -> key) = liftIO $
--  c_pressKeyDown flags key

{- for reference-parameter "getters" (unary).

-}
getByReference :: (Storable a) => (Ptr a -> IO ()) -> IO a
getByReference setReference = bracket
 malloc
 free
 (\p -> setReference p >> peek p)
