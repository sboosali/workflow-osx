{-# LANGUAGE LambdaCase #-}
module Workflow.OSX.Execute where
import Workflow.OSX.Bindings as ObjC
import Workflow.OSX.Types

import Control.Monad.Free
import Control.Monad.Trans.State

import Control.Concurrent             (threadDelay)

import Data.Foldable                  (traverse_)
import Data.List                      (intercalate)
import Data.Monoid                    ((<>))


runActions :: Actions a -> IO a
runActions = iterM $ \case
 -- iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a

 SendKeyChord    flags key k      -> ObjC.pressKey flags key >> k
 SendText        s k              -> runActions (sendTextAsKeypresses s) >> k
 -- terminates because sendTextAsKeypresses is exclusively a sequence of SendKeyChord'es

 -- TODO SendMouseClick  flags n button k -> ObjC.clickMouse flags n button >> k

 GetClipboard    f                -> ObjC.getClipboard >>= f
 SetClipboard    s k              -> ObjC.setClipboard s >> k

 CurrentApplication f             -> ObjC.currentApplication >>= f
 OpenApplication app k            -> ObjC.openApplication app >> k
 OpenURL         url k            -> ObjC.openURL url >> k

 Delay           t k              -> threadDelay (t*1000) >> k
 -- 1,000 µs is 1ms

-- | returns a sequence of 'SendKeyChord'es.
sendTextAsKeypresses :: String -> Actions ()
sendTextAsKeypresses
 = traverse_ (\(modifiers, key) -> liftF $ SendKeyChord modifiers key ())
 . concatMap char2keypress
 -- liftF :: ActionF () -> Free ActionF ()

runActionsWithDelay :: Int -> Actions a -> IO a
runActionsWithDelay t = iterM $ \case
 -- iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a

 SendKeyChord    flags key k      -> threadDelay (t*1000) >> ObjC.pressKey flags key             >> k
 SendText        s k              -> runActions (sendTextAsKeypressesWithDelay t s)              >> k

 GetClipboard    f                -> threadDelay (t*1000) >> ObjC.getClipboard                   >>= f
 SetClipboard    s k              -> threadDelay (t*1000) >> ObjC.setClipboard s                 >> k

 CurrentApplication f             -> threadDelay (t*1000) >> ObjC.currentApplication             >>= f
 OpenApplication app k            -> threadDelay (t*1000) >> ObjC.openApplication app            >> k
 OpenURL         url k            -> threadDelay (t*1000) >> ObjC.openURL url                    >> k

 Delay           t_ k              -> threadDelay (t_*1000)                                      >> k
 -- 1,000 µs is 1ms

-- | returns a sequence of 'SendKeyChord'es.
sendTextAsKeypressesWithDelay :: Int -> String -> Actions ()
sendTextAsKeypressesWithDelay t
 = traverse_ (\(modifiers, key) -> do
    liftF $ Delay t                    ()
    liftF $ SendKeyChord modifiers key ())
 . concatMap char2keypress
 -- liftF :: ActionF () -> Free ActionF ()

{- | shows the "static" data flow of some 'Actions', by showing its primitive operations, in @do-notation@.

e.g.

>>> :{
putStrLn . showActions $ do
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
showActions :: (Show x) => Actions x -> String
showActions as = "do\n" <> evalState (showActions_ as) 1

 where
 showActions_ :: (Show x) => Actions x -> State Gensym String
 showActions_ (Pure x) = return $ " return " <> show x <> "\n"
 showActions_ (Free a) = showAction_ a

 showAction_ :: (Show x) => ActionF (Actions x) -> State Gensym String
 showAction_ = \case
  SendKeyChord    flags key k -> ((" sendKeyChord "    <> showArgs [show flags, show key]) <>)       <$> showActions_ k
  -- TODO SendMouseClick  flags n b k -> ((" sendMouseClick "  <> showArgs [show flags, show n, show b]) <>) <$> showActions_ k
  SendText        s k         -> ((" sendText "        <> showArgs [show s]) <>)                     <$> showActions_ k

  SetClipboard    s k         -> ((" setClipboard "    <> showArgs [show s]) <>)                     <$> showActions_ k
  OpenApplication app k       -> ((" openApplication " <> showArgs [show app]) <>)                   <$> showActions_ k
  OpenURL         url k       -> ((" openURL "         <> showArgs [show url]) <>)                   <$> showActions_ k
  Delay           t k         -> ((" delay "           <> showArgs [show t]) <>)                     <$> showActions_ k

 -- TODO distinguish between strings and variables to avoid:
 -- x2 <- getClipboard
 -- sendText ("x2")

  GetClipboard f -> do
   x <- gensym
   rest <- showActions_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- getClipboard" <> showArgs [] <> rest

  CurrentApplication f -> do
   x <- gensym
   rest <- showActions_ (f ("{"<>x<>"}"))
   return $ " " <> x <> " <- currentApplication" <> showArgs [] <> rest

 showArgs :: [String] -> String
 showArgs xs = intercalate " " (fmap (("(" <>) . (<> ")")) xs) <> "\n"

type Gensym = Int

gensym :: State Gensym String
gensym = do
 i <- get
 put $ i + 1
 return $ "x" <> show i
