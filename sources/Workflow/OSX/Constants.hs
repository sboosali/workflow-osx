module Workflow.OSX.Constants where
import Workflow.OSX.Types

import Data.BitVector


-- | line 236 of </System/Library/Frameworks/IOKit.framework/Versions/A/Headers/hidsystem/IOLLEvent.h>
--
--
marshallMask :: Modifier -> BitVector
marshallMask CommandMod  = 0x00100000
marshallMask Control  = 0x00040000
marshallMask Shift    = 0x00020000
marshallMask Option   = 0x00080000
marshallMask Function = 0x00800000

-- yes: #define NX_CONTROLMASK 0x00040000
-- no: #define NX_DEVICELCTLKEYMASK 0x00000001
-- no: #define NX_DEVICERCTLKEYMASK 0x00002000

-- | line 196 of </System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>
--
marshallKeycode :: Key -> BitVector
marshallKeycode AKey             = 0x00
marshallKeycode SKey             = 0x01
marshallKeycode DKey             = 0x02
marshallKeycode FKey             = 0x03
marshallKeycode HKey             = 0x04
marshallKeycode GKey             = 0x05
marshallKeycode ZKey             = 0x06
marshallKeycode XKey             = 0x07
marshallKeycode CKey             = 0x08
marshallKeycode VKey             = 0x09
marshallKeycode BKey             = 0x0B
marshallKeycode QKey             = 0x0C
marshallKeycode WKey             = 0x0D
marshallKeycode EKey             = 0x0E
marshallKeycode RKey             = 0x0F
marshallKeycode YKey             = 0x10
marshallKeycode TKey             = 0x11
marshallKeycode OneKey           = 0x12
marshallKeycode TwoKey           = 0x13
marshallKeycode ThreeKey         = 0x14
marshallKeycode FourKey          = 0x15
marshallKeycode SixKey           = 0x16
marshallKeycode FiveKey          = 0x17
marshallKeycode EqualKey         = 0x18
marshallKeycode NineKey          = 0x19
marshallKeycode SevenKey         = 0x1A
marshallKeycode MinusKey         = 0x1B
marshallKeycode EightKey         = 0x1C
marshallKeycode ZeroKey          = 0x1D
marshallKeycode RightBracketKey  = 0x1E
marshallKeycode OKey             = 0x1F
marshallKeycode UKey             = 0x20
marshallKeycode LeftBracketKey   = 0x21
marshallKeycode IKey             = 0x22
marshallKeycode PKey             = 0x23
marshallKeycode LKey             = 0x25
marshallKeycode JKey             = 0x26
marshallKeycode QuoteKey         = 0x27
marshallKeycode KKey             = 0x28
marshallKeycode SemicolonKey     = 0x29
marshallKeycode BackslashKey     = 0x2A
marshallKeycode CommaKey         = 0x2B
marshallKeycode SlashKey         = 0x2C
marshallKeycode NKey             = 0x2D
marshallKeycode MKey             = 0x2E
marshallKeycode PeriodKey        = 0x2F
marshallKeycode GraveKey         = 0x32
marshallKeycode ReturnKey        = 0x24
marshallKeycode TabKey           = 0x30
marshallKeycode SpaceKey         = 0x31
marshallKeycode DeleteKey        = 0x33
marshallKeycode EscapeKey        = 0x35
marshallKeycode CommandKey       = 0x37
marshallKeycode ShiftKey         = 0x38
marshallKeycode CapsLockKey      = 0x39
marshallKeycode OptionKey        = 0x3A
marshallKeycode ControlKey       = 0x3B
marshallKeycode FunctionKey      = 0x3F
marshallKeycode F17Key           = 0x40
marshallKeycode F18Key           = 0x4F
marshallKeycode F19Key           = 0x50
marshallKeycode F20Key           = 0x5A
marshallKeycode F5Key            = 0x60
marshallKeycode F6Key            = 0x61
marshallKeycode F7Key            = 0x62
marshallKeycode F3Key            = 0x63
marshallKeycode F8Key            = 0x64
marshallKeycode F9Key            = 0x65
marshallKeycode F11Key           = 0x67
marshallKeycode F13Key           = 0x69
marshallKeycode F16Key           = 0x6A
marshallKeycode F14Key           = 0x6B
marshallKeycode F10Key           = 0x6D
marshallKeycode F12Key           = 0x6F
marshallKeycode F15Key           = 0x71
marshallKeycode ForwardDeleteKey = 0x75
marshallKeycode F4Key            = 0x76
marshallKeycode F2Key            = 0x78
marshallKeycode F1Key            = 0x7A
marshallKeycode LeftArrowKey     = 0x7B
marshallKeycode RightArrowKey    = 0x7C
marshallKeycode DownArrowKey     = 0x7D
marshallKeycode UpArrowKey       = 0x7E

