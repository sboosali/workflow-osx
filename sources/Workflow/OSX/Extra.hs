-- | assorted functionality, imported by most modules in this package.
module Workflow.OSX.Extra
 ( module Workflow.OSX.Extra
 , Data, Generic, NFData
 , (<>), traverse_
 , module X
 ) where

import Control.DeepSeq (NFData)

import Data.Data (Data)
import           GHC.Generics                 (Generic)
import           Data.Foldable                   (traverse_)
import Data.Monoid        ((<>))
import Data.List as X
import Control.Concurrent (threadDelay)
import Data.Word (Word16)


delayMilliseconds :: Int -> IO ()
delayMilliseconds t = threadDelay (t*1000)

unsafeIntToWord16 :: Int -> Word16
unsafeIntToWord16 = fromInteger . toInteger
