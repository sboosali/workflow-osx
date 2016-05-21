{-# LANGUAGE LambdaCase #-}
module Workflow.OSX.Constants where
import Workflow.OSX.Types


{- |

line 236 of
</System/Library/Frameworks/IOKit.framework/Versions/A/Headers/hidsystem/IOLLEvent.h>

-}
toCGEventFlags :: Modifier -> CGEventFlags
toCGEventFlags = \case
 CommandModifier  -> 0x00100000
 ControlModifier  -> 0x00040000
 ShiftModifier    -> 0x00020000
 OptionModifier   -> 0x00080000
 FunctionModifier -> 0x00800000

-- yes: #define NX_CONTROLMASK 0x00040000
-- no: #define NX_DEVICELCTLKEYMASK 0x00000001
-- no: #define NX_DEVICERCTLKEYMASK 0x00002000

{- |

line 196 of
</System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>

-}
toCGKeyCode :: Key -> CGKeyCode
toCGKeyCode = \case
 AKey             -> 0x00
 SKey             -> 0x01
 DKey             -> 0x02
 FKey             -> 0x03
 HKey             -> 0x04
 GKey             -> 0x05
 ZKey             -> 0x06
 XKey             -> 0x07
 CKey             -> 0x08
 VKey             -> 0x09
 BKey             -> 0x0B
 QKey             -> 0x0C
 WKey             -> 0x0D
 EKey             -> 0x0E
 RKey             -> 0x0F
 YKey             -> 0x10
 TKey             -> 0x11
 OneKey           -> 0x12
 TwoKey           -> 0x13
 ThreeKey         -> 0x14
 FourKey          -> 0x15
 SixKey           -> 0x16
 FiveKey          -> 0x17
 EqualKey         -> 0x18
 NineKey          -> 0x19
 SevenKey         -> 0x1A
 MinusKey         -> 0x1B
 EightKey         -> 0x1C
 ZeroKey          -> 0x1D
 RightBracketKey  -> 0x1E
 OKey             -> 0x1F
 UKey             -> 0x20
 LeftBracketKey   -> 0x21
 IKey             -> 0x22
 PKey             -> 0x23
 LKey             -> 0x25
 JKey             -> 0x26
 QuoteKey         -> 0x27
 KKey             -> 0x28
 SemicolonKey     -> 0x29
 BackslashKey     -> 0x2A
 CommaKey         -> 0x2B
 SlashKey         -> 0x2C
 NKey             -> 0x2D
 MKey             -> 0x2E
 PeriodKey        -> 0x2F
 GraveKey         -> 0x32
 ReturnKey        -> 0x24
 TabKey           -> 0x30
 SpaceKey         -> 0x31
 DeleteKey        -> 0x33
 EscapeKey        -> 0x35
 CommandKey       -> 0x37
 ShiftKey         -> 0x38
 CapsLockKey      -> 0x39
 OptionKey        -> 0x3A
 ControlKey       -> 0x3B
 FunctionKey      -> 0x3F
 F17Key           -> 0x40
 F18Key           -> 0x4F
 F19Key           -> 0x50
 F20Key           -> 0x5A
 F5Key            -> 0x60
 F6Key            -> 0x61
 F7Key            -> 0x62
 F3Key            -> 0x63
 F8Key            -> 0x64
 F9Key            -> 0x65
 F11Key           -> 0x67
 F13Key           -> 0x69
 F16Key           -> 0x6A
 F14Key           -> 0x6B
 F10Key           -> 0x6D
 F12Key           -> 0x6F
 F15Key           -> 0x71
 ForwardDeleteKey -> 0x75
 F4Key            -> 0x76
 F2Key            -> 0x78
 F1Key            -> 0x7A
 LeftArrowKey     -> 0x7B
 RightArrowKey    -> 0x7C
 DownArrowKey     -> 0x7D
 UpArrowKey       -> 0x7E
