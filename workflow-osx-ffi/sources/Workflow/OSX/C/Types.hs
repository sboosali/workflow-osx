{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass, PatternSynonyms, ConstraintKinds, FlexibleContexts #-}
module Workflow.OSX.C.Types where
import Workflow.OSX.C.Extra

import Foreign (Storable(..))
import Foreign.CStorable (CStorable(..))
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

-- | no modifiers
nullCGEventFlags :: CGEventFlags
nullCGEventFlags = 0

{- | an event (like key/mouse up/down).

relates a Haskell type with a Objective-C type:

* @typedef CF_ENUM(uint32_t, CGEventType) { ... }@
* in
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>

-}
type CGEventType = Word32

{- |

-}
data OSXKey = OSXKey
  { osxModifiers :: CGEventFlags
  , osxKey       :: CGKeyCode
  } deriving (Show,Read,Eq,Ord,Generic,NFData)

fromOSXKey :: OSXKey -> (CGEventFlags,CGKeyCode)
fromOSXKey (OSXKey ms k) = (ms,k)

{- | relates a Haskell type with a Objective-C type:

* @typedef CF_ENUM(uint32_t, CGMouseButton) { ... }@
* in
</System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>

-}
type CGMouseButton = Word32

{-|

-}
type OSXMouseButton = (CGMouseButton, CGEventType, CGEventType)

data OSXMouseButton' = OSXMouseButton
 { osxMouseButton :: CGMouseButton
 , osxMouseUp     :: CGEventType
 , osxMouseDown   :: CGEventType
 } deriving (Show,Read,Eq,Ord,Generic,NFData,CStorable)

-- instance Storable OSXMouseButton' where
--  peek      = cPeek
--  poke      = cPoke
--  alignment = cAlignment
--  sizeOf    = cSizeOf

{-|

@<CoreGraphics/CGBase.h>@ defines:

@
#if defined(__LP64__) && __LP64__
# define CGFLOAT_TYPE double
# define CGFLOAT_IS_DOUBLE 1
# define CGFLOAT_MIN DBL_MIN
# define CGFLOAT_MAX DBL_MAX
#else
# define CGFLOAT_TYPE float
# define CGFLOAT_IS_DOUBLE 0
# define CGFLOAT_MIN FLT_MIN
# define CGFLOAT_MAX FLT_MAX
#endif

typedef CGFLOAT_TYPE CGFloat;
@

-}
type CGFloat = Double

{-|

@<CoreGraphics/CGGeometry.h>@ defines:

@
struct CGPoint {
    CGFloat x;
    CGFloat y;
};
typedef struct CGPoint CGPoint;
@

-}
data CGPoint = CGPoint
 { xCGPoint :: CGFloat
 , yCGPoint :: CGFloat
 } deriving (Show,Read,Eq,Ord,Generic,NFData,CStorable)

instance Storable CGPoint where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf

{-

@
struct CGSize {
    CGFloat width;
    CGFloat height;
};
typedef struct CGSize CGSize;
@

-}

{-

@
struct CGVector {
    CGFloat dx;
    CGFloat dy;
};
typedef struct CGVector CGVector;
@

-}

{-

@
struct CGRect {
    CGPoint origin;
    CGSize size;
};
typedef struct CGRect CGRect;
@

-}

{-|
@
struct ProcessSerialNumber { unsigned long highLongOfPSN; unsigned long lowLongOfPSN; };
@

<https://developer.apple.com/legacy/library/documentation/Carbon/Reference/Process_Manager/index.html#//apple_ref/doc/c_ref/ProcessSerialNumber>

-}
data ProcessSerialNumber = ProcessSerialNumber
 { _highLongOfPSN :: CULong
 , _lowLongOfPSN  :: CULong
 } deriving (Show,Read,Eq,Ord,Generic,NFData,CStorable)

instance Storable ProcessSerialNumber where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf

{-|

E.G. In Objective-C:

 @
 // via NSLog(@"%@", ...);

 NSDictionary: {
 NSApplicationBundleIdentifier = "org.gnu.Emacs";
 NSApplicationName = Emacs;
 NSApplicationPath = "/Applications/Notes.app";
 NSApplicationProcessIdentifier = 40831;
 NSApplicationProcessSerialNumberHigh = 0;
 NSApplicationProcessSerialNumberLow = 4195328;
 NSWorkspaceApplicationKey = "<NSRunningApplication: 0x7fe3c0e08b30 (org.gnu.Emacs - 40831)>";
 }
 @

-}
data ApplicationInformation = ApplicationInformation
  { nsApplicationName :: String
  , nsApplicationPath :: String
  , nsApplicationBundleIdentifier :: String
  , nsApplicationProcessIdentifier :: Word --TODO
  , nsApplicationProcessSerialNumber :: ProcessSerialNumber
  , nsWorkspaceApplicationKey :: NSRunningApplication --TODO
  } deriving (Show,Read,Eq,Ord,Generic,NFData)

type NSRunningApplication = () --TODO

---

type ApplicationName = String

type ClipboardText = String
