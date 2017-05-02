-- | assorted functionality, imported by most modules in this package.
module Workflow.OSX.C.Extra
 ( module Workflow.OSX.C.Extra
 , module Prelude.Spiros
 ) where

import Control.Concurrent (threadDelay)
import Data.Word (Word16,Word32)
import Foreign
import Foreign.C

import qualified Prelude
import Prelude.Spiros

-- | pauses the thread for that many `ms`
delayMilliseconds :: Int -> IO ()
delayMilliseconds t = threadDelay (t*1000)

-- | may overflow.
unsafeIntToWord16 :: Int -> Word16
unsafeIntToWord16 = fromInteger . toInteger

maxWord16 :: Word16
maxWord16 = maxBound - 1

-- | may get capped.
roundingNatToWord32 :: Natural -> Word32
roundingNatToWord32 = fromInteger . min maxWord16 . toInteger

-- holdKeyFor :: (MonadIO m) => Int -> [Modifier] -> Key -> m ()
-- holdKeyFor milliseconds (marshallModifiers -> flags) (marshallKey -> key) = liftIO $
--  c_pressKeyDown flags key

{- | for reference-parameter "getters", unary.

-}
getByReference :: (Storable a) => (Ptr a -> IO ()) -> IO a
getByReference setReference = bracket
 malloc
 free
 (\p -> setReference p >> peek p)

