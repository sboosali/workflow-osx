-- | assorted functionality, imported by most modules in this package.  
module Workflow.OSX.Extra 
 ( module Workflow.OSX.Extra 
 , Data, Generic, NFData
 , (<>), traverse_
 ) where

import           Control.Monad.Catch          (MonadThrow, throwM)
import Control.DeepSeq (NFData) 

import Data.Data (Data) 
import           GHC.Generics                 (Generic)
import           Data.Foldable                   (traverse_)
import Data.Monoid        ((<>))


failed :: (MonadThrow m) => String -> m a
failed = throwM . userError

