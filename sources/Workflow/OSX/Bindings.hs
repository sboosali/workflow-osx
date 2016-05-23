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


sendText :: String -> IO ()
-- sendText :: (MonadIO m) => String -> m ()
sendText s = sequence_ $ intersperse (delayMilliseconds 30) (fmap sendChar s)

{-| Haskell chars are Unicode code-points:

>>> import Data.Char (ord)
>>> map ord "abc"
[97,98,99]
>>> map ord "αβγ"
[945,946,947]
>>> [0x03B1,0x03B2,0x03B3]
[945,946,947]

TODO there are 1,000,000 hs chars, but UniChar only holds 2^16 ~ 65,000

-}
sendChar :: Char -> IO ()
sendChar c = c_sendChar (unsafeIntToWord16 . ord $ c)
-- sendChar :: (MonadIO m) => Char -> m ()
-- sendChar c = liftIO $ c_sendChar (ord c)

currentApplication :: IO Application
currentApplication = do -- TODO munge, default to Global
 path <- currentApplicationPath
 return path

-- |
-- TODO Applications whose name/paths have Unicode characters may or may not marshall correctly. they should, unless we need CWString.
currentApplicationPath :: IO String
currentApplicationPath = c_currentApplicationPath >>= peekCString

-- |
pressKey :: [Modifier] -> Key -> IO ()
pressKey (marshallModifiers -> flags) (marshallKey -> key) =
 c_pressKey flags key

-- TODO |
-- clickMouse :: [Modifier] -> Positive -> MouseButton -> IO ()
-- clickMouse (MouseClick (marshallModifiers -> flags) (marshallPositive -> n) (marshallButton -> button)) = c_clickMouse

-- |
getClipboard :: IO ClipboardText
getClipboard = c_getClipboard >>= peekCString

-- |
--
-- note: unlike the keyboard shortcuts of 'copy',
-- contents don't show up in Alfred's clipboard history.
setClipboard :: ClipboardText -> IO ()
setClipboard s = withCString s c_setClipboard

-- |
openURL :: URL -> IO ()
openURL s = withCString s c_openURL

-- |
openApplication :: Application -> IO ()
openApplication s = withCString s c_openApplication

-- holdKeyFor :: Int -> [Modifier] -> Key -> IO ()
-- holdKeyFor milliseconds (marshallModifiers -> flags) (marshallKey -> key) =
--  c_pressKeyDown flags key
