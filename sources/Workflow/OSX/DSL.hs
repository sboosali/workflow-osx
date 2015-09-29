{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns                   #-}
module Workflow.OSX.DSL where
import           Workflow.OSX.Types

import           Control.Monad.Free          (MonadFree, liftF)
import           Control.Monad.Free.TH       (makeFree)
import qualified Data.ByteString.Char8       as BS
import           Network.HTTP.Types.URI      (renderQuery)

import           Data.Monoid                 ((<>))


makeFree ''ActionF

insert :: (MonadAction m) => String -> m ()
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

-- I want "type = mapM_ (press . KeyChord [] . key)" to be "atomic" i.e. no delay between each step. I want "press (KeyChord [Command] RKey) >> type text" to be "laggy" i.e. some delay between each step. If I "instrument" some "Actions", by interleaving "Wait 25" between each step i.e. each "Free _", I can't distinguish between groups of steps. Thus, I should manually insert "Wait 25" between any steps that need some lag, or "automate it locally" in helper functions, but not "automate it globally" by interleaving.

copy :: (MonadAction m) => m String
copy = do
 sendKeyChord [CommandMod] CKey
 delay 100 -- TODO how long does it need to wait?
 getClipboard

paste :: (MonadAction m) => m ()
paste = do
 sendKeyChord [CommandMod] VKey

google :: (MonadAction m) => String -> m ()
google (BS.pack -> query) = openURL (BS.unpack $ "https://www.google.com/search" <> renderQuery True [("q", Just query)])

-- TODO
-- instance convert KeyChord Actions
-- instance convert MouseClick Actions

-- doubleClick :: AMonadAction_
-- doubleClick = clickMouse [] LeftButton 2

-- rightClick :: AMonadAction_
-- rightClick = clickMouse [] RightButton 1

