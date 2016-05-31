{-# LANGUAGE DeriveAnyClass, PatternSynonyms, ConstraintKinds, FlexibleContexts #-}
module Workflow.OSX.Types
 ( module Workflow.OSX.Types
 , module Workflow.Types
 ) where

import Workflow.Types

import Foreign.C.Types
import Data.Word


{-|

relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned int UInt32;@
* in @MacTypes.h@

-}
type UInt32 = Word32

{-| a UTF-16 encoded unicode character.

TODO i.e. not codepoint like Char?

@
typedef unsigned short unichar;
@

-}
type UniChar = Word16

{- | a virtual key.

relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned short uint16_t;@
* line 34 of
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGRemoteOperation.h>
defines @typedef uint16_t CGKeyCode;@

-}
type CGKeyCode = CUShort

{- | a set of modifiers.

relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned long long uint64_t;@
* line 98 of
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>
defines @typedef uint64_t CGEventFlags;@
(@typedef CF_ENUM(uint64_t, CGEventFlags)@).

-}
type CGEventFlags  = CULLong

{- | an event (like key/mouse up/down).

relates a Haskell type with a Objective-C type:

* @typedef CF_ENUM(uint32_t, CGEventType) { ... }@
* in
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>

-}
type CGEventType = Word32

{- | relates a Haskell type with a Objective-C type:

* @typedef CF_ENUM(uint32_t, CGMouseButton) { ... }@
* in
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>

-}
type CGMouseButton = Word32

--------------------------------------------------------------------------------
-- compat

-- | an (unordered, no-duplicates) sequence of key chords make up a keyboard shortcut
type KeyRiff  = KeySequence

type ClipboardText = Clipboard

pattern CommandModifier :: Modifier
pattern CommandModifier = MetaModifier -- and Hyper

pattern CommandKey :: Key
pattern CommandKey = MetaKey -- and Hyper
