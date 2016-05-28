{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts, RecordWildCards #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Bindings as Cocoa
import Workflow.OSX.Types

import Workflow.Core

import Control.Monad.Free
import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO

import Control.Concurrent             (threadDelay)
import Control.Monad.IO.Class


runWorkflow :: Workflow a -> IO a
runWorkflow = runWorkflowT . toFreeT

runWorkflowWithDelay :: Int -> Workflow a -> IO a
runWorkflowWithDelay t = runWorkflowT . delayWorkflowT t . toFreeT

runWorkflowWithDelayT :: (MonadIO m) => Int -> WorkflowT m a -> m a
runWorkflowWithDelayT t = runWorkflowT . delayWorkflowT t

{-|

you can eliminate a custom monad:

@
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
runWorkflowT :: forall m a. (MonadIO m) => WorkflowT m a -> m a
runWorkflowT = runWorkflowByT osxWorkflowD

{-|

-}
osxWorkflowD :: (MonadIO m) => WorkflowD m
osxWorkflowD = WorkflowD{..}
 where

 _sendKeyChord = Cocoa.pressKey
 _sendText     = Cocoa.sendText

 _sendMouseClick  = error "TODO: Cocoa.clickMouse"
 _sendMouseScroll = error "TODO: Cocoa.scrollMouse"

 _getClipboard = Cocoa.getClipboard
 _setClipboard = Cocoa.setClipboard

 _currentApplication = Cocoa.currentApplication
 _openApplication    = Cocoa.openApplication
 _openURL            = Cocoa.openURL
