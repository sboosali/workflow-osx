{-# LANGUAGE ForeignFunctionInterface #-}
{-| low-level bindings, with C types.

-}
module Workflow.OSX.Foreign where
import Workflow.OSX.Types

import Foreign.C.String            (CString)
import Foreign.C.Types             (CULLong (..), CUShort (..))
-- import Data.Word (Word32)


foreign import ccall safe "workflow.h sendUnichar"
 c_sendChar :: UniChar -> IO ()

foreign import ccall safe "workflow.h currentApplicationPath" c_currentApplicationPath
 :: IO CString

foreign import ccall safe "workflow.h pressKey"               c_pressKey
 :: CGEventFlags
 -> CGKeyCode
 -> IO ()

-- foreign import ccall safe "workflow.h clickMouse"             c_clickMouse
--  :: CGEventType
--  -> CGEventFlags
--  -> CGEventType
--  -> CGMouseButton
--  -> Word32
--  -> IO ()

foreign import ccall safe "workflow.h getClipboard"           c_getClipboard
 :: IO CString

foreign import ccall safe "workflow.h setClipboard"           c_setClipboard
 :: CString
 -> IO ()

foreign import ccall safe "workflow.h openURL"                c_openURL
 :: CString
 -> IO ()

foreign import ccall safe "workflow.h openApplication"        c_openApplication
 :: CString
 -> IO ()
