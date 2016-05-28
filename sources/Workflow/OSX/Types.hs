{-# LANGUAGE DeriveAnyClass, PatternSynonyms, ConstraintKinds, FlexibleContexts #-}
module Workflow.OSX.Types
 ( module Workflow.OSX.Types
 , module Workflow.Types
 ) where

import Workflow.Types

import Foreign.C.Types
import Data.Word


{-|

@
typedef unsigned short unichar;
@

-}
type UniChar = Word16

{- | relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned short uint16_t;@
* line 34 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGRemoteOperation.h> defines @typedef uint16_t CGKeyCode;@

-}
type CGKeyCode     = CUShort --TODO newtype

{- | relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned long long uint64_t;@
* line 98 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h> defines @typedef uint64_t CGEventFlags;@

-}
type CGEventFlags  = CULLong --TODO newtype

-- {- | relates a Haskell type with a Objective-C type:


-- -}
-- type CGEventType   =

-- {- | relates a Haskell type with a Objective-C type:


-- -}
-- type CGMouseButton =


-- data MouseClick = MouseClick [Modifier] Positive MouseButton
--  deriving (Show,Eq,Ord)

-- data MouseButton = LeftButton | MiddleButton | RightButton
--  deriving (Show,Eq,Ord,Enum,Bounded)

-- data KeyChord = KeyChord [Modifier] Key
--  deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------
-- compat

-- | an (unordered, no-duplicates) sequence of key chords make up a keyboard shortcut
type KeyRiff  = KeySequence

type ClipboardText = Clipboard

pattern CommandModifier :: Modifier
pattern CommandModifier = MetaModifier -- and Hyper

pattern CommandKey :: Key
pattern CommandKey = MetaKey -- and Hyper
