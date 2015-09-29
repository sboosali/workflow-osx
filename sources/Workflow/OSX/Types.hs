{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric, PatternSynonyms, ConstraintKinds, FlexibleContexts #-}
module Workflow.OSX.Types where
import Workflow.OSX.Extra

import Control.Monad.Free (MonadFree, Free)
import           Control.Monad.Catch          (MonadThrow)
import Foreign.C.Types



-- | a monad constraint for "workflow effects", (just like @MonadState@ is a monad constraint for "state effects") can use in any monad transformer stack that handles them.
type MonadAction = MonadFree ActionF

-- {- | for convenience. 
-- without loss of generality (I don't think) when declaring simple monadic effects (like Kleisli arrows). 

-- e.g.

-- @
-- getClipboard :: 'AMonadAction'      String  -- generalized
-- getClipboard :: ('MonadAction' m) => m String  -- generalized
-- getClipboard :: Free 'ActionF'         String  -- specialized
-- @

-- -}
-- type AMonadAction a = forall m. (MonadAction m) => m a

-- type AMonadAction_ = forall m. (MonadAction m) => m ()

-- | a platform-agnostic free monad, which can be executed by platform-specific bindings.
type Actions = Free ActionF

type Actions_ = Actions ()

-- | the "Action Functor".
data ActionF k
 = SendKeyChord    [Modifier] Key                   k
 | SendText        String                           k -- ^ a logical grouping for debugging and optimizing
 --TODO | SendMouseClick  [Modifier] Int MouseButton  k

 | GetClipboard                                     (ClipboardText -> k)
 | SetClipboard    ClipboardText                k

 | CurrentApplication                               (Application -> k)
 | OpenApplication Application                      k

 --TODO | CurrentWindow                               (Window -> k)
 --TODO | OpenWindow Window                      k

 | OpenURL         URL                              k

 | Delay           Time                             k
 -- TODO  | Annihilate      SomeException                       -- no k, it annihilates the action, for mzero and MonadThrow
 -- TODO   | PerformIO       (IO a)                           (a -> k)
 deriving (Functor)
 -- deriving (Functor,Data)

-- data ActionF mod key k  TODO platform-specific keyboards 
--  = SendKeyChord    [mod] key      k 
-- TODO convert between keyboards, like Alt on Windows/Linux being Command on OSX 

type ClipboardText = String
-- newtype ClipboardText = ClipboardText String  deriving (Show,Eq,Ord,IsString)

type Application = String
-- newtype Application = Application String  deriving (Show,Eq,Ord,IsString)

type URL = String
-- newtype URL = URL String  deriving (Show,Eq,Ord,IsString)

type Time = Int
-- newtype Time = Time String  deriving (Show,Eq,Ord,Num)
-- units package

-- class IsString TODO needs Free ActionF, which must be lifted, which isn't better than insert 


{- | relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned short uint16_t;@
* line 34 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGRemoteOperation.h> defines @typedef uint16_t CGKeyCode;@

-}
type CGKeyCode     = CUShort

{- | relates a Haskell type with a Objective-C type:

* Objective-C defines @typedef unsigned long long uint64_t;@
* line 98 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h> defines @typedef uint64_t CGEventFlags;@

-}
type CGEventFlags  = CULLong

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

--TODO | a (pseudo)-refinement type.
-- newtype Positive = Positive { getPositive :: Int } deriving (Show,Eq,Ord,Data,Generic,Num) -- TODO or just use Natural? 

-- -- | smart constructor for 'Positive'.
-- newPositive :: Int -> Possibly Positive
-- newPositive i = if i >= 1
--  then return (Positive i)
--  else failure 'newPositive


-- {- |

-- @Press [Command, Shift] AKey@ is easy to read, and
-- @Press []@ is natural to partially apply

-- -}
-- data KeyChord = KeyChord [Modifier] Key
--  deriving (Show,Eq,Ord)

type KeyRiff  = [KeyChord]
type KeyChord = ([Modifier], Key)
pattern KeyChord ms k = (ms, k)
pattern NoMod       k = ([],   k)

addMod :: Modifier -> KeyChord -> KeyChord
addMod m (ms, k) = KeyChord (m:ms) k
-- false positive nonexhaustive warning with the KeyChord pattern 

{- | modifier keys are keys that can be "held".

the escape key is "pressed", not "held", it seems.
(possibly explains its behavior in your terminal emulator?)

@alt@ is 'Option'.

-}
data Modifier = Control | CommandMod | Shift | Option | Function
 -- Command is qualified to not conflict with Commands.Command.Types
 deriving (Show,Eq,Ord,Bounded,Enum,Data,Generic)
-- data Modifier = ControlMod | CommandMod | ShiftMod | OptionMod | FunctionMod

{- | all the keys on a standard keyboard.


-}
data Key

 = CommandKey
 | ControlKey
 | CapsLockKey
 | ShiftKey
 | OptionKey
 | FunctionKey

 | GraveKey
 | MinusKey
 | EqualKey
 | DeleteKey
 | ForwardDeleteKey
 | LeftBracketKey
 | RightBracketKey
 | BackslashKey
 | SemicolonKey
 | QuoteKey
 | CommaKey
 | PeriodKey
 | SlashKey

 | TabKey
 | SpaceKey
 | ReturnKey

 | LeftArrowKey
 | RightArrowKey
 | DownArrowKey
 | UpArrowKey

 | AKey
 | BKey
 | CKey
 | DKey
 | EKey
 | FKey
 | GKey
 | HKey
 | IKey
 | JKey
 | KKey
 | LKey
 | MKey
 | NKey
 | OKey
 | PKey
 | QKey
 | RKey
 | SKey
 | TKey
 | UKey
 | VKey
 | WKey
 | XKey
 | YKey
 | ZKey

 | ZeroKey
 | OneKey
 | TwoKey
 | ThreeKey
 | FourKey
 | FiveKey
 | SixKey
 | SevenKey
 | EightKey
 | NineKey

 | EscapeKey
 | F1Key
 | F2Key
 | F3Key
 | F4Key
 | F5Key
 | F6Key
 | F7Key
 | F8Key
 | F9Key
 | F10Key
 | F11Key
 | F12Key
 | F13Key
 | F14Key
 | F15Key
 | F16Key
 | F17Key
 | F18Key
 | F19Key
 | F20Key

 deriving (Show,Eq,Ord,Bounded,Enum,Data,Generic)

{- |

>>> int2keypress -12
[([],MinusKey),([],OneKey),([],TwoKey)]


-}
int2keypress :: Integer -> [KeyChord]
int2keypress = concatMap char2keypress . show

{- | 

a (base ten) digit is a number between zero and nine inclusive.

>>> digit2keypress 2
([],TwoKey)

>>> digit2keypress -2
Nothing

>>> digit2keypress 12
Nothing

-}
digit2keypress :: (MonadThrow m) => Integer -> m KeyChord
digit2keypress 0  = return $ NoMod ZeroKey
digit2keypress 1  = return $ NoMod OneKey
digit2keypress 2  = return $ NoMod TwoKey
digit2keypress 3  = return $ NoMod ThreeKey
digit2keypress 4  = return $ NoMod FourKey
digit2keypress 5  = return $ NoMod FiveKey
digit2keypress 6  = return $ NoMod SixKey
digit2keypress 7  = return $ NoMod SevenKey
digit2keypress 8  = return $ NoMod EightKey
digit2keypress 9  = return $ NoMod NineKey

digit2keypress k = failed $ "digit2keypress: digits must be between zero and nine: " <> show k


{- | the keypress that would insert the character into the application.

>>> char2keypress '@' :: Maybe KeyChord
Just ([Shift], TwoKey)

some characters cannot be represented as keypresses, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keypress '\0' :: Maybe KeyChord
Nothing

prop> case char2keypress c of {  Just ([],_) -> True;  Just ([Shift],_) -> True;  Nothing -> True;  _ -> False  }

-}
char2keypress :: (MonadThrow m) => Char -> m KeyChord -- ((,) [Modifier] Key)

char2keypress 'a'  = return $ (,) [     ] AKey
char2keypress 'A'  = return $ (,) [Shift] AKey
char2keypress 'b'  = return $ (,) [     ] BKey
char2keypress 'B'  = return $ (,) [Shift] BKey
char2keypress 'c'  = return $ (,) [     ] CKey
char2keypress 'C'  = return $ (,) [Shift] CKey
char2keypress 'd'  = return $ (,) [     ] DKey
char2keypress 'D'  = return $ (,) [Shift] DKey
char2keypress 'e'  = return $ (,) [     ] EKey
char2keypress 'E'  = return $ (,) [Shift] EKey
char2keypress 'f'  = return $ (,) [     ] FKey
char2keypress 'F'  = return $ (,) [Shift] FKey
char2keypress 'g'  = return $ (,) [     ] GKey
char2keypress 'G'  = return $ (,) [Shift] GKey
char2keypress 'h'  = return $ (,) [     ] HKey
char2keypress 'H'  = return $ (,) [Shift] HKey
char2keypress 'i'  = return $ (,) [     ] IKey
char2keypress 'I'  = return $ (,) [Shift] IKey
char2keypress 'j'  = return $ (,) [     ] JKey
char2keypress 'J'  = return $ (,) [Shift] JKey
char2keypress 'k'  = return $ (,) [     ] KKey
char2keypress 'K'  = return $ (,) [Shift] KKey
char2keypress 'l'  = return $ (,) [     ] LKey
char2keypress 'L'  = return $ (,) [Shift] LKey
char2keypress 'm'  = return $ (,) [     ] MKey
char2keypress 'M'  = return $ (,) [Shift] MKey
char2keypress 'n'  = return $ (,) [     ] NKey
char2keypress 'N'  = return $ (,) [Shift] NKey
char2keypress 'o'  = return $ (,) [     ] OKey
char2keypress 'O'  = return $ (,) [Shift] OKey
char2keypress 'p'  = return $ (,) [     ] PKey
char2keypress 'P'  = return $ (,) [Shift] PKey
char2keypress 'q'  = return $ (,) [     ] QKey
char2keypress 'Q'  = return $ (,) [Shift] QKey
char2keypress 'r'  = return $ (,) [     ] RKey
char2keypress 'R'  = return $ (,) [Shift] RKey
char2keypress 's'  = return $ (,) [     ] SKey
char2keypress 'S'  = return $ (,) [Shift] SKey
char2keypress 't'  = return $ (,) [     ] TKey
char2keypress 'T'  = return $ (,) [Shift] TKey
char2keypress 'u'  = return $ (,) [     ] UKey
char2keypress 'U'  = return $ (,) [Shift] UKey
char2keypress 'v'  = return $ (,) [     ] VKey
char2keypress 'V'  = return $ (,) [Shift] VKey
char2keypress 'w'  = return $ (,) [     ] WKey
char2keypress 'W'  = return $ (,) [Shift] WKey
char2keypress 'x'  = return $ (,) [     ] XKey
char2keypress 'X'  = return $ (,) [Shift] XKey
char2keypress 'y'  = return $ (,) [     ] YKey
char2keypress 'Y'  = return $ (,) [Shift] YKey
char2keypress 'z'  = return $ (,) [     ] ZKey
char2keypress 'Z'  = return $ (,) [Shift] ZKey

char2keypress '0'  = return $ (,) [     ] ZeroKey
char2keypress ')'  = return $ (,) [Shift] ZeroKey
char2keypress '1'  = return $ (,) [     ] OneKey
char2keypress '!'  = return $ (,) [Shift] OneKey
char2keypress '2'  = return $ (,) [     ] TwoKey
char2keypress '@'  = return $ (,) [Shift] TwoKey
char2keypress '3'  = return $ (,) [     ] ThreeKey
char2keypress '#'  = return $ (,) [Shift] ThreeKey
char2keypress '4'  = return $ (,) [     ] FourKey
char2keypress '$'  = return $ (,) [Shift] FourKey
char2keypress '5'  = return $ (,) [     ] FiveKey
char2keypress '%'  = return $ (,) [Shift] FiveKey
char2keypress '6'  = return $ (,) [     ] SixKey
char2keypress '^'  = return $ (,) [Shift] SixKey
char2keypress '7'  = return $ (,) [     ] SevenKey
char2keypress '&'  = return $ (,) [Shift] SevenKey
char2keypress '8'  = return $ (,) [     ] EightKey
char2keypress '*'  = return $ (,) [Shift] EightKey
char2keypress '9'  = return $ (,) [     ] NineKey
char2keypress '('  = return $ (,) [Shift] NineKey

char2keypress '`'  = return $ (,) [     ] GraveKey
char2keypress '~'  = return $ (,) [Shift] GraveKey
char2keypress '-'  = return $ (,) [     ] MinusKey
char2keypress '_'  = return $ (,) [Shift] MinusKey
char2keypress '='  = return $ (,) [     ] EqualKey
char2keypress '+'  = return $ (,) [Shift] EqualKey
char2keypress '['  = return $ (,) [     ] LeftBracketKey
char2keypress '{'  = return $ (,) [Shift] LeftBracketKey
char2keypress ']'  = return $ (,) [     ] RightBracketKey
char2keypress '}'  = return $ (,) [Shift] RightBracketKey
char2keypress '\\' = return $ (,) [     ] BackslashKey
char2keypress '|'  = return $ (,) [Shift] BackslashKey
char2keypress ';'  = return $ (,) [     ] SemicolonKey
char2keypress ':'  = return $ (,) [Shift] SemicolonKey
char2keypress '\'' = return $ (,) [     ] QuoteKey
char2keypress '"'  = return $ (,) [Shift] QuoteKey
char2keypress ','  = return $ (,) [     ] CommaKey
char2keypress '<'  = return $ (,) [Shift] CommaKey
char2keypress '.'  = return $ (,) [     ] PeriodKey
char2keypress '>'  = return $ (,) [Shift] PeriodKey
char2keypress '/'  = return $ (,) [     ] SlashKey
char2keypress '?'  = return $ (,) [Shift] SlashKey

char2keypress ' '  = return $ (,) [     ] SpaceKey
char2keypress '\t' = return $ (,) [     ] TabKey
char2keypress '\n' = return $ (,) [     ] ReturnKey

char2keypress c = failed $ "char2keypress: un-pressable Char: " <> show c


{- | the character that represents the keypress:

>>> keypress2char ([Shift], TwoKey) :: Maybe Char
Just '@'

some keypresses cannot be represented as characters, like keyboard shortcuts:

>>> keypress2char ([Command], CKey) :: Maybe Char
Nothing

>>> import Data.Char
>>> import Data.Maybe
prop> maybe True isAscii (keypress2char k)
TODO replace true with redo test

-}
keypress2char :: (MonadThrow m) => KeyChord -> m Char -- ((,) [Modifier] Key)

keypress2char ((,) [     ] AKey)            = return $ 'a'
keypress2char ((,) [Shift] AKey)            = return $ 'A'
keypress2char ((,) [     ] BKey)            = return $ 'b'
keypress2char ((,) [Shift] BKey)            = return $ 'B'
keypress2char ((,) [     ] CKey)            = return $ 'c'
keypress2char ((,) [Shift] CKey)            = return $ 'C'
keypress2char ((,) [     ] DKey)            = return $ 'd'
keypress2char ((,) [Shift] DKey)            = return $ 'D'
keypress2char ((,) [     ] EKey)            = return $ 'e'
keypress2char ((,) [Shift] EKey)            = return $ 'E'
keypress2char ((,) [     ] FKey)            = return $ 'f'
keypress2char ((,) [Shift] FKey)            = return $ 'F'
keypress2char ((,) [     ] GKey)            = return $ 'g'
keypress2char ((,) [Shift] GKey)            = return $ 'G'
keypress2char ((,) [     ] HKey)            = return $ 'h'
keypress2char ((,) [Shift] HKey)            = return $ 'H'
keypress2char ((,) [     ] IKey)            = return $ 'i'
keypress2char ((,) [Shift] IKey)            = return $ 'I'
keypress2char ((,) [     ] JKey)            = return $ 'j'
keypress2char ((,) [Shift] JKey)            = return $ 'J'
keypress2char ((,) [     ] KKey)            = return $ 'k'
keypress2char ((,) [Shift] KKey)            = return $ 'K'
keypress2char ((,) [     ] LKey)            = return $ 'l'
keypress2char ((,) [Shift] LKey)            = return $ 'L'
keypress2char ((,) [     ] MKey)            = return $ 'm'
keypress2char ((,) [Shift] MKey)            = return $ 'M'
keypress2char ((,) [     ] NKey)            = return $ 'n'
keypress2char ((,) [Shift] NKey)            = return $ 'N'
keypress2char ((,) [     ] OKey)            = return $ 'o'
keypress2char ((,) [Shift] OKey)            = return $ 'O'
keypress2char ((,) [     ] PKey)            = return $ 'p'
keypress2char ((,) [Shift] PKey)            = return $ 'P'
keypress2char ((,) [     ] QKey)            = return $ 'q'
keypress2char ((,) [Shift] QKey)            = return $ 'Q'
keypress2char ((,) [     ] RKey)            = return $ 'r'
keypress2char ((,) [Shift] RKey)            = return $ 'R'
keypress2char ((,) [     ] SKey)            = return $ 's'
keypress2char ((,) [Shift] SKey)            = return $ 'S'
keypress2char ((,) [     ] TKey)            = return $ 't'
keypress2char ((,) [Shift] TKey)            = return $ 'T'
keypress2char ((,) [     ] UKey)            = return $ 'u'
keypress2char ((,) [Shift] UKey)            = return $ 'U'
keypress2char ((,) [     ] VKey)            = return $ 'v'
keypress2char ((,) [Shift] VKey)            = return $ 'V'
keypress2char ((,) [     ] WKey)            = return $ 'w'
keypress2char ((,) [Shift] WKey)            = return $ 'W'
keypress2char ((,) [     ] XKey)            = return $ 'x'
keypress2char ((,) [Shift] XKey)            = return $ 'X'
keypress2char ((,) [     ] YKey)            = return $ 'y'
keypress2char ((,) [Shift] YKey)            = return $ 'Y'
keypress2char ((,) [     ] ZKey)            = return $ 'z'
keypress2char ((,) [Shift] ZKey)            = return $ 'Z'

keypress2char ((,) [     ] ZeroKey)         = return $ '0'
keypress2char ((,) [Shift] ZeroKey)         = return $ ')'
keypress2char ((,) [     ] OneKey)          = return $ '1'
keypress2char ((,) [Shift] OneKey)          = return $ '!'
keypress2char ((,) [     ] TwoKey)          = return $ '2'
keypress2char ((,) [Shift] TwoKey)          = return $ '@'
keypress2char ((,) [     ] ThreeKey)        = return $ '3'
keypress2char ((,) [Shift] ThreeKey)        = return $ '#'
keypress2char ((,) [     ] FourKey)         = return $ '4'
keypress2char ((,) [Shift] FourKey)         = return $ '$'
keypress2char ((,) [     ] FiveKey)         = return $ '5'
keypress2char ((,) [Shift] FiveKey)         = return $ '%'
keypress2char ((,) [     ] SixKey)          = return $ '6'
keypress2char ((,) [Shift] SixKey)          = return $ '^'
keypress2char ((,) [     ] SevenKey)        = return $ '7'
keypress2char ((,) [Shift] SevenKey)        = return $ '&'
keypress2char ((,) [     ] EightKey)        = return $ '8'
keypress2char ((,) [Shift] EightKey)        = return $ '*'
keypress2char ((,) [     ] NineKey)         = return $ '9'
keypress2char ((,) [Shift] NineKey)         = return $ '('

keypress2char ((,) [     ] GraveKey)        = return $ '`'
keypress2char ((,) [Shift] GraveKey)        = return $ '~'
keypress2char ((,) [     ] MinusKey)        = return $ '-'
keypress2char ((,) [Shift] MinusKey)        = return $ '_'
keypress2char ((,) [     ] EqualKey)        = return $ '='
keypress2char ((,) [Shift] EqualKey)        = return $ '+'
keypress2char ((,) [     ] LeftBracketKey)  = return $ '['
keypress2char ((,) [Shift] LeftBracketKey)  = return $ '{'
keypress2char ((,) [     ] RightBracketKey) = return $ ']'
keypress2char ((,) [Shift] RightBracketKey) = return $ '}'
keypress2char ((,) [     ] BackslashKey)    = return $ '\\'
keypress2char ((,) [Shift] BackslashKey)    = return $ '|'
keypress2char ((,) [     ] SemicolonKey)    = return $ ';'
keypress2char ((,) [Shift] SemicolonKey)    = return $ ':'
keypress2char ((,) [     ] QuoteKey)        = return $ '\''
keypress2char ((,) [Shift] QuoteKey)        = return $ '"'
keypress2char ((,) [     ] CommaKey)        = return $ ','
keypress2char ((,) [Shift] CommaKey)        = return $ '<'
keypress2char ((,) [     ] PeriodKey)       = return $ '.'
keypress2char ((,) [Shift] PeriodKey)       = return $ '>'
keypress2char ((,) [     ] SlashKey)        = return $ '/'
keypress2char ((,) [Shift] SlashKey)        = return $ '?'

keypress2char ((,) [     ] SpaceKey)        = return $ ' '
keypress2char ((,) [     ] TabKey)          = return $ '\t'
keypress2char ((,) [     ] ReturnKey)       = return $ '\n'

keypress2char k = failed $ "keypress2char: non-unicode-representable Keypress: " <> show k

-- TODO partial isomorphism between char2keypress and keypress2char?
