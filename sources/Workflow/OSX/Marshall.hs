{-# LANGUAGE LambdaCase #-}
{-|

glue the "virtual" virtual keys ('Key' and 'Modifier')
to "actual" virtual keys (operating system constants).

-}
module Workflow.OSX.Marshall where
import Workflow.OSX.Constants
import Workflow.OSX.Types

import Data.Bits


{- | Marshall the bitmask.

relates Haskell types with Objective-C types:

* Haskell list-of-enum ~ Objective-C bit-mask
* Haskell @['Modifier']@ ~ Objective-C @CGEventFlags@
* Haskell 'CULLong' ~ Objective-C @uint64_t@
* Haskell can marshal 'CULLong'


= Implementation

"Bit-vectors are interpreted as unsigned integers (i.e. natural numbers)"

since each bit'mask' is disjoint, and we logical-OR the bits
together, we don't need to remove duplicates.

-}
marshallModifiers :: [Modifier] -> CGEventFlags
marshallModifiers
 = foldl (.|.) zeroBits
 . fmap toCGEventFlags

{- |
-}
toCGEventFlags :: Modifier -> CGEventFlags
toCGEventFlags = \case
  CommandModifier  -> NX_COMMANDMASK
  ControlModifier  -> NX_CONTROLMASK
  ShiftModifier    -> NX_SHIFTMASK
  OptionModifier   -> NX_ALTERNATEMASK
  FunctionModifier -> NX_SECONDARYFNMASK

{- | marshall the 'keycode'

relates Haskell types with Objective-C types:

* Haskell 'Key' ~ Objective-C @CGKeyCode@
* Haskell 'CUShort' ~ Objective-C @uint16_t@
* Haskell can marshal 'CUShort'

-}
marshallKey :: Key -> CGKeyCode
marshallKey
 = toCGKeyCode

{- |

-}
toCGKeyCode :: Key -> CGKeyCode
toCGKeyCode = \case

 AKey             -> VK_ANSI_A
 SKey             -> VK_ANSI_S
 DKey             -> VK_ANSI_D
 FKey             -> VK_ANSI_F
 HKey             -> VK_ANSI_H
 GKey             -> VK_ANSI_G
 ZKey             -> VK_ANSI_Z
 XKey             -> VK_ANSI_X
 CKey             -> VK_ANSI_C
 VKey             -> VK_ANSI_V
 BKey             -> VK_ANSI_B
 QKey             -> VK_ANSI_Q
 WKey             -> VK_ANSI_W
 EKey             -> VK_ANSI_E
 RKey             -> VK_ANSI_R
 YKey             -> VK_ANSI_Y
 TKey             -> VK_ANSI_T
 OneKey           -> VK_ANSI_1
 TwoKey           -> VK_ANSI_2
 ThreeKey         -> VK_ANSI_3
 FourKey          -> VK_ANSI_4
 SixKey           -> VK_ANSI_6
 FiveKey          -> VK_ANSI_5
 EqualKey         -> VK_ANSI_Equal
 NineKey          -> VK_ANSI_9
 SevenKey         -> VK_ANSI_7
 MinusKey         -> VK_ANSI_Minus
 EightKey         -> VK_ANSI_8
 ZeroKey          -> VK_ANSI_0
 RightBracketKey  -> VK_ANSI_RightBracket
 OKey             -> VK_ANSI_O
 UKey             -> VK_ANSI_U
 LeftBracketKey   -> VK_ANSI_LeftBracket
 IKey             -> VK_ANSI_I
 PKey             -> VK_ANSI_P
 LKey             -> VK_ANSI_L
 JKey             -> VK_ANSI_J
 QuoteKey         -> VK_ANSI_Quote
 KKey             -> VK_ANSI_K
 SemicolonKey     -> VK_ANSI_Semicolon
 BackslashKey     -> VK_ANSI_Backslash
 CommaKey         -> VK_ANSI_Comma
 SlashKey         -> VK_ANSI_Slash
 NKey             -> VK_ANSI_N
 MKey             -> VK_ANSI_M
 PeriodKey        -> VK_ANSI_Period
 GraveKey         -> VK_ANSI_Grave

 ReturnKey        -> VK_Return
 TabKey           -> VK_Tab
 SpaceKey         -> VK_Space
 DeleteKey        -> VK_Delete
 EscapeKey        -> VK_Escape
 CommandKey       -> VK_Command
 ShiftKey         -> VK_Shift
 CapsLockKey      -> VK_CapsLock
 OptionKey        -> VK_Option
 ControlKey       -> VK_Control
 FunctionKey      -> VK_Function

 F17Key           -> VK_F17
 F18Key           -> VK_F18
 F19Key           -> VK_F19
 F20Key           -> VK_F20
 F5Key            -> VK_F5
 F6Key            -> VK_F6
 F7Key            -> VK_F7
 F3Key            -> VK_F3
 F8Key            -> VK_F8
 F9Key            -> VK_F9
 F11Key           -> VK_F11
 F13Key           -> VK_F13
 F16Key           -> VK_F16
 F14Key           -> VK_F14
 F10Key           -> VK_F10
 F12Key           -> VK_F12
 F15Key           -> VK_F15
 ForwardDeleteKey -> VK_ForwardDelete
 F4Key            -> VK_F4
 F2Key            -> VK_F2
 F1Key            -> VK_F1
 LeftArrowKey     -> VK_LeftArrow
 RightArrowKey    -> VK_RightArrow
 DownArrowKey     -> VK_DownArrow
 UpArrowKey       -> VK_UpArrow
