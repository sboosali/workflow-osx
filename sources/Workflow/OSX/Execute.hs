{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Bindings as Cocoa
import Workflow.OSX.Types
-- import Workflow.Types
-- import Workflow.Execute

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

{-| intersperse a delay between each action.

@
delayWorkflowT 1 $ do
 sendKeyChord [CommandModifier] VKey
 s <- getClipboard
 sendText s
@

is equivalent to:

@
do
 sendKeyChord [CommandModifier] VKey
 delay 1
 s <- getClipboard
 delay 1
 sendText s
@

-}
delayWorkflowT :: (Monad m) => Int -> WorkflowT m a -> WorkflowT m a
delayWorkflowT t = intersperseT (Delay t ())

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
-- runWorkflowT = runWorkflowByT osxDictionary
runWorkflowT = iterT go

 where
 go :: WorkflowF (m a) -> m a
 go = \case

  SendKeyChord    flags key k      -> liftIO (Cocoa.pressKey flags key) >> k
  SendText        s k              -> liftIO (Cocoa.sendText s) >> k
  -- TODO support Unicode by inserting "directly"
  -- terminates because sendTextAsKeypresses is exclusively a sequence of SendKeyChord'es

  -- TODO SendMouseClick  flags n button k -> Cocoa.clickMouse flags n button >> k

  GetClipboard    f                -> liftIO (Cocoa.getClipboard) >>= f
  SetClipboard    s k              -> liftIO (Cocoa.setClipboard s) >> k

  CurrentApplication f             -> liftIO (Cocoa.currentApplication) >>= f
  OpenApplication app k            -> liftIO (Cocoa.openApplication app) >> k
  OpenURL         url k            -> liftIO (Cocoa.openURL url) >> k

  Delay           t k              -> liftIO (threadDelay (t*1000)) >> k
 -- 1,000 µs is 1ms

-- osxDictionary :: (MonadIO m) => WorkflowD m
-- osxDictionary = WorkflowD{..}
--  where
--  _getClipboard =
--  _setClipboard s =
