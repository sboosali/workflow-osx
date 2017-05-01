-- | assorted functionality, imported by most modules in this package.
module Workflow.OSX.Extra
 ( module Workflow.OSX.Extra
 , module X
 ) where
import Workflow.Types (MilliSeconds)

import Control.DeepSeq as X (NFData)
import Data.Hashable as X (Hashable)
import Data.String.Conv as X

import Data.Data as X (Data)
import           GHC.Generics               as X  (Generic)
import           Data.Foldable                  as X (traverse_)
import Data.Monoid        as X ((<>))
import Data.List as X
import Control.Arrow as X ((>>>))
import Data.Maybe        as X (catMaybes)
import Numeric.Natural as X (Natural)

import Control.Concurrent (threadDelay)
import Data.Word (Word16,Word32)
import qualified Turtle as SH
import Prelude hiding (FilePath)
import qualified Prelude

delayMilliseconds :: Int -> IO ()
delayMilliseconds t = threadDelay (t*1000)

-- | may overflow.
unsafeIntToWord16 :: Int -> Word16
unsafeIntToWord16 = fromInteger . toInteger

-- | may overflow.
unsafeNatToWord32 :: Natural -> Word32
unsafeNatToWord32 = fromInteger . toInteger

nat2ms :: Natural -> MilliSeconds
nat2ms = fromIntegral

fp2s :: SH.FilePath -> Prelude.FilePath
fp2s = fp2t >>> toS

fp2t :: SH.FilePath -> SH.Text
fp2t = SH.format SH.fp
