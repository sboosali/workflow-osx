-- | assorted functionality, imported by most modules in this package.
module Workflow.OSX.Extra
 ( module Workflow.OSX.Extra
 , (<>), traverse_
 , module X
 ) where

import Control.DeepSeq as X (NFData)
import Data.Hashable as X (Hashable)

import Data.Data as X (Data)
import           GHC.Generics               as X  (Generic)
import           Data.Foldable                   (traverse_)
import Data.Monoid        ((<>))
import Data.List as X
import Control.Concurrent (threadDelay)
import Data.Word (Word16,Word32)
import Numeric.Natural (Natural)


delayMilliseconds :: Int -> IO ()
delayMilliseconds t = threadDelay (t*1000)

-- | may overflow.
unsafeIntToWord16 :: Int -> Word16
unsafeIntToWord16 = fromInteger . toInteger

-- | may overflow.
unsafeNatToWord32 :: Natural -> Word32
unsafeNatToWord32 = fromInteger . toInteger
