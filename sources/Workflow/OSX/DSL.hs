{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns                   #-}
-- | some higher-level workflows you can derive from the primitives in 'Workflow'. (also see the source)
module Workflow.OSX.DSL where
import           Workflow.OSX.Types

import           Control.Monad.Free          (MonadFree, liftF)
import           Control.Monad.Free.TH       (makeFree)
import qualified Data.ByteString.Char8       as BS
import           Network.HTTP.Types.URI      (renderQuery)

import           Data.Monoid                 ((<>))


makeFree ''WorkflowF

insert :: (MonadWorkflow m) => String -> m ()
insert = sendText

-- TODO
-- wait (25 ms)
-- wait $ 25 ms
-- wait (25ms)
--
-- ms :: TimeUnit
-- instance Num (TimeUnit -> Time)
-- instance Num (UnitOf a -> a)
--
-- module Commands.Compiler.Sugar where

-- I want "type = mapM_ (press . KeyChord [] . key)" to be "atomic" i.e. no delay between each step. I want "press (KeyChord [Command] RKey) >> type text" to be "laggy" i.e. some delay between each step. If I "instrument" some "Workflow", by interleaving "Wait 25" between each step i.e. each "Free _", I can't distinguish between groups of steps. Thus, I should manually insert "Wait 25" between any steps that need some lag, or "automate it locally" in helper functions, but not "automate it globally" by interleaving.

-- | access the currently selected region from Haskell, via the clipboard  
copy :: (MonadWorkflow m) => m String
copy = do
 sendKeyChord [CommandModifier] CKey
 delay 100 -- TODO how long does it need to wait?
 getClipboard

paste :: (MonadWorkflow m) => m ()
paste = do
 sendKeyChord [CommandModifier] VKey

-- | google a query. properly encodes the url. 
google :: (MonadWorkflow m) => String -> m ()
google (BS.pack -> query) = openURL (BS.unpack $ "https://www.google.com/search" <> renderQuery True [("q", Just query)])

-- TODO
-- instance convert KeyChord Workflow
-- instance convert MouseClick Workflow

-- doubleClick :: AMonadWorkflow_
-- doubleClick = clickMouse [] LeftButton 2

-- rightClick :: AMonadWorkflow_
-- rightClick = clickMouse [] RightButton 1

