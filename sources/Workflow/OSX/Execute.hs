{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Bindings as ObjC
import Workflow.OSX.Types

import Control.Monad.Free
import Control.Monad.Trans.Free hiding (Pure, Free, iterM) -- TODO  
import Control.Monad.Trans.State

import Control.Concurrent             (threadDelay)

import Data.Foldable                  (traverse_)
import Data.List                      (intercalate)
import Data.Monoid                    ((<>))
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
runWorkflowT = iterT go
 where 
 go :: WorkflowF (m a) -> m a
 go = \case

  SendKeyChord    flags key k      -> runSendKeyChord flags key >> k
  SendText        s k              -> runSendText s >> k
  -- TODO support Unicode by inserting "directly" 
  -- terminates because sendTextAsKeypresses is exclusively a sequence of SendKeyChord'es

  -- TODO SendMouseClick  flags n button k -> ObjC.clickMouse flags n button >> k

  GetClipboard    f                -> liftIO (ObjC.getClipboard) >>= f
  SetClipboard    s k              -> liftIO (ObjC.setClipboard s) >> k

  CurrentApplication f             -> liftIO (ObjC.currentApplication) >>= f
  OpenApplication app k            -> liftIO (ObjC.openApplication app) >> k
  OpenURL         url k            -> liftIO (ObjC.openURL url) >> k

  Delay           t k              -> liftIO (threadDelay (t*1000)) >> k
 -- 1,000 µs is 1ms

{-| 

TODO support Unicode by inserting directly, not indirectly via keypresses  
https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/EventOverview/HandlingKeyEvents/HandlingKeyEvents.html
Most key events—that is, those representing characters to be inserted as text—are dispatched by the NSWindow object associated with the key window to the first responder.
characters and charactersIgnoringModifiers—The responder can extract the Unicode character data associated with the event and insert it as text or interpret it as commands. The charactersIgnoringModifiers method ignores any modifier keystroke (except for Shift) when returning the character data. Note that both method names are plural because a keystroke can produce more than one character (for example, “à” is composed of ‘a’ and ‘`‘).
isARepeat—This method tells the responder whether the same key was pressed rapidly in succession.

-}
runSendText :: (MonadIO m) => String -> m () 
runSendText 
 = traverse_ (uncurry runSendKeyChord)
 . concatMap char2keypress

runSendKeyChord :: (MonadIO m) => [Modifier] -> Key -> m () 
runSendKeyChord flags key = liftIO $ ObjC.pressKey flags key

{- | shows the "static" data flow of some 'Workflow', by showing its primitive operations, in @do-notation@.

e.g.

>>> :{
putStrLn . showWorkflow $ do
 sendKeyChord [Command, Shift] BKey
 delay 1000
 sendKeyChord [Command] DownArrowKey
 x1 <- currentApplication
 x2 <- getClipboard
 openURL $ "https://www.google.com/search?q=" <> x2
 setClipboard x1
 getClipboard
:}
do
 sendKeyChord ([Command,Shift]) (BKey)
 delay (1000)
 sendKeyChord ([Command]) (DownArrowKey)
 x1 <- currentApplication
 x2 <- getClipboard
 openURL ("https://www.google.com/search?q={x2}")
 setClipboard ("{x1}")
 x3 <- getClipboard
 return "{x3}"

(note: doesn't print variables as raw strings (cf. 'print' versus 'putStrLn'), as it doesn't "crystallize" all operations into "symbols", but gives you an idea of the data flow. however, it does correctly track the control flow, even when the variables are used non-sequentially.)

(note: the variables in the code were named to be consistent with
'gensym', for readability. but of course the bindings aren't reified,
and they could have been named anything)

basically, the monadically-bound variable @x1@ is shown as if it were literally @"{x1}"@ (rather than, the current clipboard contents). a more complicated alternative could be to purely model the state: e.g. a clipboard, with 'SetClipboard' and 'GetClipboard' working together, etc.).

-}
showWorkflow :: (Show x) => Workflow x -> String
showWorkflow as = "do\n" <> evalState (showWorkflow_ as) 1

 where
 showWorkflow_ :: (Show x) => Workflow x -> State Gensym String
 showWorkflow_ (Pure x) = return $ " return " <> show x <> "\n"
 showWorkflow_ (Free a) = showWorkflowF a

 showWorkflowF :: (Show x) => WorkflowF (Workflow x) -> State Gensym String
 showWorkflowF = \case
  SendKeyChord    flags key k -> ((" sendKeyChord "    <> showArgs [show flags, show key]) <>)       <$> showWorkflow_ k
  -- TODO SendMouseClick  flags n b k -> ((" sendMouseClick "  <> showArgs [show flags, show n, show b]) <>) <$> showWorkflow_ k
  SendText        s k         -> ((" sendText "        <> showArgs [show s]) <>)                     <$> showWorkflow_ k

  SetClipboard    s k         -> ((" setClipboard "    <> showArgs [show s]) <>)                     <$> showWorkflow_ k
  OpenApplication app k       -> ((" openApplication " <> showArgs [show app]) <>)                   <$> showWorkflow_ k
  OpenURL         url k       -> ((" openURL "         <> showArgs [show url]) <>)                   <$> showWorkflow_ k
  Delay           t k         -> ((" delay "           <> showArgs [show t]) <>)                     <$> showWorkflow_ k

 -- TODO distinguish between strings and variables to avoid:
 -- x2 <- getClipboard
 -- sendText ("x2")

  GetClipboard f -> do
   x <- gensym
   rest <- showWorkflow_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- getClipboard" <> showArgs [] <> rest

  CurrentApplication f -> do
   x <- gensym
   rest <- showWorkflow_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- currentApplication" <> showArgs [] <> rest

 showArgs :: [String] -> String
 showArgs xs = intercalate " " (fmap (("(" <>) . (<> ")")) xs) <> "\n"

 gensym :: State Int String
 gensym = do
  i <- get
  put $ i + 1
  return $ "x" <> show i

type Gensym = Int

