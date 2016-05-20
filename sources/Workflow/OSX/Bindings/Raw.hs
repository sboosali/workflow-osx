{-# LANGUAGE ForeignFunctionInterface #-}
module Workflow.OSX.Bindings.Raw where
import Workflow.OSX.Types

import Foreign.C.String            (CString)
import Foreign.C.Types             (CULLong (..), CUShort (..))
-- import Data.Word (Word32)


foreign import ccall safe "workflow.h currentApplicationPath" objc_currentApplicationPath
 :: IO CString

foreign import ccall safe "workflow.h pressKey"               objc_pressKey
 :: CGEventFlags
 -> CGKeyCode
 -> IO ()

-- foreign import ccall safe "workflow.h clickMouse"             objc_clickMouse
--  :: CGEventType
--  -> CGEventFlags
--  -> CGEventType
--  -> CGMouseButton
--  -> Word32
--  -> IO ()

foreign import ccall safe "workflow.h getClipboard"           objc_getClipboard
 :: IO CString

foreign import ccall safe "workflow.h setClipboard"           objc_setClipboard
 :: CString
 -> IO ()

foreign import ccall safe "workflow.h openURL"                objc_openURL
 :: CString
 -> IO ()

foreign import ccall safe "workflow.h openApplication"        objc_openApplication
 :: CString
 -> IO ()
