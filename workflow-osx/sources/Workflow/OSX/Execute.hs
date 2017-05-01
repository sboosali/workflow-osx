{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Extra
import Workflow.OSX.Bindings as Cocoa
import Workflow.OSX.Types

import Workflow.Core

import Data.Default.Class

import Control.Monad.Free
import Control.Monad.Trans.Free (intersperseT)

import Control.Monad.IO.Class

--------------------------------------------------------------------------------

{-|

All delays are in milliseconds.

-}
data OSXWorkflowConfig = OSXWorkflowConfig
 { osxHowToSendText :: HowToSendText
 , osxStepDelay     :: Natural
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData OSXWorkflowConfig

-- | 'defaultOSXWorkflowConfig'
instance Default OSXWorkflowConfig where
   def = defaultOSXWorkflowConfig

{- | How to execute 'sendText'.

Comparison:

* SendTextByChar:
* SendTextByKey:
* SendTextByClipboard:

-}
data HowToSendText = SendTextByChar | SendTextByKey | SendTextByClipboard
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData HowToSendText

-- | 'defaultHowToSendText'
instance Default HowToSendText where
   def = defaultHowToSendText

{-|@
'osxHowToSendText' = 'defaultOSXHowToSendText'
'osxStepDelay'     = 'defaultOSXStepDelay'
@-}
defaultOSXWorkflowConfig :: OSXWorkflowConfig
defaultOSXWorkflowConfig = OSXWorkflowConfig{..}
 where
 osxHowToSendText = defaultHowToSendText
 osxStepDelay     = defaultOSXStepDelay

{-|

@
= 'SendTextByKey'
@-}
defaultHowToSendText :: HowToSendText
defaultHowToSendText = SendTextByKey

{-| no delay.

@=0@
-}
defaultOSXStepDelay :: Natural
defaultOSXStepDelay = 0

--------------------------------------------------------------------------------

{- | A natural transformation from workflows to io.

Default settings and no transformers, for convenience.

@= 'runWorkflow' 'defaultOSXWorkflowConfig'@

-}
runWorkflowDefault :: Workflow a -> IO a
runWorkflowDefault = runWorkflow defaultOSXWorkflowConfig

-- | @runWorkflow config = 'runWorkflowT' config . 'toFreeT'@
runWorkflow :: OSXWorkflowConfig -> Workflow a -> IO a
runWorkflow config = runWorkflowT config . toFreeT

{-|

you can eliminate a custom monad:

@
{# LANGUAGE GeneralizedNewtypeDeriving #}

newtype W a = W
 { getW :: WorkflowT IO a
 } deriving
 ( MonadWorkflow
 , MonadIO
 , Monad
 , Applicative
 , Functor
 )
@

with:

@
runW :: W a -> IO a
runW = 'runMonadWorkflow' . getW
@

-}
runWorkflowT :: forall m a. (MonadIO m) => OSXWorkflowConfig -> WorkflowT m a -> m a
runWorkflowT config@OSXWorkflowConfig{..}
   = _delay
 >>> runWorkflowByT _dictionary

 where
 _dictionary = osxWorkflowD config
 _delay = case osxStepDelay of
   0 -> id -- optimization
   t -> intersperseT (Delay (nat2ms t) ())  -- (`delay` is too general, requiring unnecessary constraints)

{-|

-}
osxWorkflowD :: (MonadIO m) => OSXWorkflowConfig -> WorkflowD m
osxWorkflowD OSXWorkflowConfig{..} = WorkflowD{..} --TODO use delays
 where

 _sendText = case osxHowToSendText of
   SendTextByChar      -> Cocoa.sendText_byChar
   SendTextByKey       -> Cocoa.sendText_byKey
   SendTextByClipboard -> Cocoa.sendText_byClipboard

 _sendKeyChord = Cocoa.sendKeyChord_flags

 _sendMouseClick  = Cocoa.sendMouseClick
 _sendMouseScroll = error "TODO: Cocoa.sendMouseScroll"

 _getClipboard = Cocoa.getClipboard
 _setClipboard = Cocoa.setClipboard

 _currentApplication = Cocoa.currentApplication
 _openApplication    = Cocoa.openApplication
 _openURL            = Cocoa.openURL

--------------------------------------------------------------------------------
