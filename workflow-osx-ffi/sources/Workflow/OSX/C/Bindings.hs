{-# LANGUAGE ViewPatterns #-}
{-| low-level bindings, with Haskell types.

There are three ways to insert text into the currently-active application:

* 'insertByChar'
* 'insertByKey'
* 'insertByClipboard'

all have different tradeoffs, which they document.

-}
module Workflow.OSX.C.Bindings where
import Workflow.OSX.C.Extra
import Workflow.OSX.C.Foreign
import Workflow.OSX.C.Types hiding (setClipboard)
import Workflow.Keys (char2keychord)

import Foreign
import Foreign.C
import Data.Char (ord)
import Control.Monad.IO.Class
import Numeric.Natural
import Control.Exception (bracket,bracket_)

--------------------------------------------------------------------------------
-- Workflow types

{- |

Problem: crashes some apps (chrome and atom "unexpectedly quit" (...both use chrome), emacs hangs). 

-}
insertByChar :: (MonadIO m) => String -> m ()
-- insert :: (MonadIO m) => (Monad m  m) => String -> m ()
insertByChar s = liftIO $
 sequence_ $ intersperse (delayMilliseconds 30) (fmap sendChar s)
 --NOTE must pause between events

{- |

Problem: partial (ignores Unicode). 

-}
insertByKey :: (MonadIO m) => String -> m ()
insertByKey
    = fmap char2osxkey
  >>> catMaybes
  >>> traverse_ (uncurry c_pressKey)

{- |

Problems: pollutes clipboard, requires variable delays, race conditions, requires (hardcoded) "Command-v" hotkey, ...

-}
insertByClipboard :: (MonadIO m) => String -> m ()
insertByClipboard s = do
  setClipboard s
  liftIO $ delayMilliseconds 30
  c_pressKey (NX_COMMANDMASK) VK_ANSI_V

--------------------------------------------------------------------------------

{-| Haskell chars are Unicode code-points:

PARTIAL: silently ignores codepoints beyond 2^16.

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
 -- c_sendChar (unsafeIntToWord16 . ord $ c) --TODO
 if o <= maxWord16
 then c_sendChar (unsafeIntToWord16 o)
 else return ()
 where
 o = ord c

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
 (roundingNatToWord32 -> times) --TODO bound
 (button,downEvent,upEvent)
 (CGPoint x y)
 = liftIO $ do
     c_clickMouseAt modifiers times button downEvent upEvent x y

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

-- pressKey :: (MonadIO m) => CGKeyCode -> m ()
-- pressKey key = liftIO $ do
--  pressKeyDown key
--  pressKeyUp   key

pressKeyChord :: (MonadIO m) => [CGKeyCode] -> CGKeyCode -> m () --TODO rn KeyChord
pressKeyChord modifiers key = liftIO $ do
  holdingKeys modifiers $ do
      pressKeyDown key
      pressKeyUp   key

{- perform an action while holding down some keys (e.g. modifiers).

via 'bracket_', the keys are released even when an exception is raised.

-}
holdingKeys :: [CGKeyCode] -> IO () -> IO ()
holdingKeys keys = bracket_
 (pressKeyDown `traverse_`         keys)
 (pressKeyUp   `traverse_` reverse keys)

-- holdingKeys :: (MonadIO m) => [CGKeyCode] -> m () -> m ()
-- holdingKeys keys action = liftIO $ bracket_
--  (pressKeyDown `traverse_` keys)
--  (pressKeyUp   `traverse_` keys)
--  action

pressKeyDown :: CGKeyCode -> IO ()
pressKeyDown = c_pressKeyDown 0

pressKeyUp :: CGKeyCode -> IO ()
pressKeyUp = c_pressKeyUp 0

--------------------------------------------------------------------------------

{- | the keychord that would insert the character into the application.

PARTIAL: 'Just' only on characters that are both ASCII and printable

>>> char2keychord '@' :: Maybe KeyChord
Just ([ShiftModifier], TwoKey)

some characters cannot be represented as keypresses, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keychord '\0' :: Maybe KeyChord
Nothing


-}
char2osxkey :: Char -> Maybe OSXKey
char2osxkey c = case c of

 'a'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_A
 'A'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_A
 'b'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_B
 'B'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_B
 'c'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_C
 'C'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_C
 'd'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_D
 'D'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_D
 'e'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_E
 'E'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_E
 'f'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_F
 'F'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_F
 'g'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_G
 'G'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_G
 'h'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_H
 'H'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_H
 'i'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_I
 'I'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_I
 'j'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_J
 'J'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_J
 'k'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_K
 'K'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_K
 'l'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_L
 'L'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_L
 'm'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_M
 'M'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_M
 'n'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_N
 'N'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_N
 'o'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_O
 'O'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_O
 'p'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_P
 'P'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_P
 'q'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Q
 'Q'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Q
 'r'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_R
 'R'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_R
 's'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_S
 'S'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_S
 't'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_T
 'T'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_T
 'u'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_U
 'U'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_U
 'v'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_V
 'V'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_V
 'w'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_W
 'W'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_W
 'x'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_X
 'X'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_X
 'y'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Y
 'Y'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Y
 'z'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Z
 'Z'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Z

 '0'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_0
 ')'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_0
 '1'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_1
 '!'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_1
 '2'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_2
 '@'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_2
 '3'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_3
 '#'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_3
 '4'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_4
 '$'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_4
 '5'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_5
 '%'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_5
 '6'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_6
 '^'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_6
 '7'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_7
 '&'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_7
 '8'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_8
 '*'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_8
 '9'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_9
 '('  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_9

 '`'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Grave
 '~'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Grave
 '-'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Minus
 '_'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Minus
 '='  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Equal
 '+'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Equal
 '['  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_LeftBracket
 '{'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_LeftBracket
 ']'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_RightBracket
 '}'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_RightBracket
 '\\' -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Backslash
 '|'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Backslash
 ';'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Semicolon
 ':'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Semicolon
 '\'' -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Quote
 '"'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Quote
 ','  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Comma
 '<'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Comma
 '.'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Period
 '>'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Period
 '/'  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Slash
 '?'  -> Just $ OSXKey (NX_SHIFTMASK    ) VK_ANSI_Slash
 ' '  -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Space
 '\t' -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Tab
 '\n' -> Just $ OSXKey (nullCGEventFlags) VK_ANSI_Return

 _    -> Nothing -- failed $ "{{ char2keychord "++(show c)++" }} not an ASCII, printable character"

