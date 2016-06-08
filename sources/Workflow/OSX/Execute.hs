{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Extra
import Workflow.OSX.Bindings as Cocoa
import Workflow.OSX.Types

import Workflow.Core

import Data.Default.Class

import Control.Monad.Free

import Control.Monad.IO.Class

--------------------------------------------------------------------------------

data OSXWorkflowConfig = OSXWorkflowConfig
 { osxHowToSendText :: HowToSendText
 , osxDelay         :: Natural
 }
 deriving (Show,Read,Eq,Ord,Data,Generic)
instance NFData OSXWorkflowConfig

{- | How to execute 'sendText'.

Comparison:

* SendTextByChar:
* SendTextByKey:
* SendTextByClipboard:

-}
data HowToSendText = SendTextByChar | SendTextByKey | SendTextByClipboard
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData HowToSendText

-- | 'defaultOSXWorkflowConfig'
instance Default OSXWorkflowConfig where def = defaultOSXWorkflowConfig

{-|@
'osxHowToSendText' = 'SendTextByKey'
'osxDelay'         = 0
@-}
defaultOSXWorkflowConfig :: OSXWorkflowConfig
defaultOSXWorkflowConfig = OSXWorkflowConfig{..}
 where
 osxHowToSendText = SendTextByKey
 osxDelay         = 0

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
runWorkflowT config = runWorkflowByT _dictionary
 where
 _dictionary = osxWorkflowD config

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
