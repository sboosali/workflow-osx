{-# LANGUAGE ForeignFunctionInterface #-}
{-| low-level bindings, with C types.

-}
module Workflow.OSX.Foreign where
import Workflow.OSX.Types

import Foreign.C.String            (CString)
import Foreign.C.Types             (CULLong (..), CUShort (..))
-- import Data.Word (Word32)


foreign import ccall safe "Workflow.h sendUnichar"
 c_sendChar :: UniChar -> IO ()

foreign import ccall safe "Workflow.h currentApplicationPath" c_currentApplicationPath
 :: IO CString

foreign import ccall safe "Workflow.h pressKey"               c_pressKey
 :: CGEventFlags
 -> CGKeyCode
 -> IO ()

-- foreign import ccall safe "Workflow.h clickMouseAt"           c_clickMouseAt
--  :: CGEventFlags
--  -> UInt32
--  -> CGMouseButton
--  -> CGEventType
--  -> CGEventType
--  -> CGPoint
--  -> IO ()

foreign import ccall safe "Workflow.h clickMouse"             c_clickMouse
 :: CGEventFlags
 -> UInt32
 -> CGMouseButton
 -> CGEventType
 -> CGEventType
 -> IO ()

foreign import ccall safe "Workflow.h getClipboard"           c_getClipboard
 :: IO CString

foreign import ccall safe "Workflow.h setClipboard"           c_setClipboard
 :: CString
 -> IO ()

foreign import ccall safe "Workflow.h openURL"                c_openURL
 :: CString
 -> IO ()

foreign import ccall safe "Workflow.h openApplication"        c_openApplication
 :: CString
 -> IO ()
