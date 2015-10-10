{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric, PatternSynonyms, ConstraintKinds, FlexibleContexts #-}
module Workflow.OSX.Types where
import Workflow.OSX.Extra

import Control.Monad.Free (MonadFree, Free)
import Control.Monad.Free.Church  (F)
import           Control.Monad.Catch          (MonadThrow)
import Foreign.C.Types



-- | a monad constraint for "workflow effects", (just like @MonadState@ is a monad constraint for "state effects") can use in any monad transformer stack that handles them.
type MonadWorkflow = MonadFree WorkflowF

-- {- | for convenience. 
-- without loss of generality (I don't think) when declaring simple monadic effects (like Kleisli arrows). 

-- e.g.

-- @
-- getClipboard :: 'AMonadWorkflow'      String  -- generalized
-- getClipboard :: ('MonadWorkflow' m) => m String  -- generalized
-- getClipboard :: Free 'WorkflowF'         String  -- specialized
-- @

-- -}
-- type AMonadWorkflow a = forall m. (MonadWorkflow m) => m a

-- type AMonadWorkflow_ = forall m. (MonadWorkflow m) => m ()

-- | a platform-agnostic free monad, which can be executed by platform-specific bindings.
type Workflow = Free WorkflowF

type Workflow_ = Workflow ()

-- | church-encoded
type CWorkflow = F WorkflowF

type CWorkflow_ = CWorkflow ()

-- | the "Workflow Functor".
data WorkflowF k
 = SendKeyChord    [Modifier] Key                   k
 | SendText        String                           k -- ^ a logical grouping for debugging and optimizing

 | GetClipboard                                     (ClipboardText -> k)
 | SetClipboard    ClipboardText                k

 | CurrentApplication                               (Application -> k)
 | OpenApplication Application                      k

 --TODO | SendMouseClick  [Modifier] Int MouseButton  k
 --TODO | CurrentWindow                               (Window -> k)
 --TODO | OpenWindow Window                      k

 | OpenURL         URL                              k

 | Delay           Time                             k
 -- TODO  | Annihilate      SomeException                       -- no k, it annihilates the action, for mzero and MonadThrow
 -- TODO   | PerformIO       (IO a)                           (a -> k)
 deriving (Functor)
 -- deriving (Functor,Data)

-- data WorkflowF mod key k  TODO platform-specific keyboards 
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

-- class IsString TODO needs Free WorkflowF, which must be lifted, which isn't better than insert 


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

-- data KeyChord = KeyChord [Modifier] Key
--  deriving (Show,Eq,Ord)

-- | an (unordered, no-duplicates) sequence of key chords make up a keyboard shortcut 
type KeyRiff  = [KeyChord]

-- | 
type KeyChord = ([Modifier], Key)

-- | @pattern KeyChord ms k = (ms,k)@ 
pattern KeyChord ms k = (ms, k)

-- | @pattern NoMod k = ([],k)@ 
pattern NoMod       k = ([],   k)

-- | appends a modifier
addMod :: Modifier -> KeyChord -> KeyChord
addMod m (ms, k) = KeyChord (m:ms) k
-- false positive nonexhaustive warning with the KeyChord pattern 

{- | modifier keys are keys that can be "held".

the escape key is "pressed", not "held", it seems.
(possibly explains its behavior in your terminal emulator?)

@alt@ is 'OptionModifier'.

-}
data Modifier = ControlModifier | CommandModifier | ShiftModifier | OptionModifier | FunctionModifier
 -- Command is qualified to not conflict with Commands.Command.Types
 deriving (Show,Eq,Ord,Bounded,Enum,Data,Generic)
-- data Modifier = ControlMod | CommandMod | ShiftMod | OptionMod | FunctionMod

{- | all the keys on a standard Apple keyboard.


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
Just ([ShiftModifier], TwoKey)

some characters cannot be represented as keypresses, like some non-printable characters
(in arbitrary applications, not just the terminal emulator):

>>> char2keypress '\0' :: Maybe KeyChord
Nothing

prop> case char2keypress c of {  Just ([],_) -> True;  Just ([ShiftModifier],_) -> True;  Nothing -> True;  _ -> False  }

-}
char2keypress :: (MonadThrow m) => Char -> m KeyChord -- ((,) [Modifier] Key)

char2keypress 'a'  = return $ (,) [     ] AKey
char2keypress 'A'  = return $ (,) [ShiftModifier] AKey
char2keypress 'b'  = return $ (,) [     ] BKey
char2keypress 'B'  = return $ (,) [ShiftModifier] BKey
char2keypress 'c'  = return $ (,) [     ] CKey
char2keypress 'C'  = return $ (,) [ShiftModifier] CKey
char2keypress 'd'  = return $ (,) [     ] DKey
char2keypress 'D'  = return $ (,) [ShiftModifier] DKey
char2keypress 'e'  = return $ (,) [     ] EKey
char2keypress 'E'  = return $ (,) [ShiftModifier] EKey
char2keypress 'f'  = return $ (,) [     ] FKey
char2keypress 'F'  = return $ (,) [ShiftModifier] FKey
char2keypress 'g'  = return $ (,) [     ] GKey
char2keypress 'G'  = return $ (,) [ShiftModifier] GKey
char2keypress 'h'  = return $ (,) [     ] HKey
char2keypress 'H'  = return $ (,) [ShiftModifier] HKey
char2keypress 'i'  = return $ (,) [     ] IKey
char2keypress 'I'  = return $ (,) [ShiftModifier] IKey
char2keypress 'j'  = return $ (,) [     ] JKey
char2keypress 'J'  = return $ (,) [ShiftModifier] JKey
char2keypress 'k'  = return $ (,) [     ] KKey
char2keypress 'K'  = return $ (,) [ShiftModifier] KKey
char2keypress 'l'  = return $ (,) [     ] LKey
char2keypress 'L'  = return $ (,) [ShiftModifier] LKey
char2keypress 'm'  = return $ (,) [     ] MKey
char2keypress 'M'  = return $ (,) [ShiftModifier] MKey
char2keypress 'n'  = return $ (,) [     ] NKey
char2keypress 'N'  = return $ (,) [ShiftModifier] NKey
char2keypress 'o'  = return $ (,) [     ] OKey
char2keypress 'O'  = return $ (,) [ShiftModifier] OKey
char2keypress 'p'  = return $ (,) [     ] PKey
char2keypress 'P'  = return $ (,) [ShiftModifier] PKey
char2keypress 'q'  = return $ (,) [     ] QKey
char2keypress 'Q'  = return $ (,) [ShiftModifier] QKey
char2keypress 'r'  = return $ (,) [     ] RKey
char2keypress 'R'  = return $ (,) [ShiftModifier] RKey
char2keypress 's'  = return $ (,) [     ] SKey
char2keypress 'S'  = return $ (,) [ShiftModifier] SKey
char2keypress 't'  = return $ (,) [     ] TKey
char2keypress 'T'  = return $ (,) [ShiftModifier] TKey
char2keypress 'u'  = return $ (,) [     ] UKey
char2keypress 'U'  = return $ (,) [ShiftModifier] UKey
char2keypress 'v'  = return $ (,) [     ] VKey
char2keypress 'V'  = return $ (,) [ShiftModifier] VKey
char2keypress 'w'  = return $ (,) [     ] WKey
char2keypress 'W'  = return $ (,) [ShiftModifier] WKey
char2keypress 'x'  = return $ (,) [     ] XKey
char2keypress 'X'  = return $ (,) [ShiftModifier] XKey
char2keypress 'y'  = return $ (,) [     ] YKey
char2keypress 'Y'  = return $ (,) [ShiftModifier] YKey
char2keypress 'z'  = return $ (,) [     ] ZKey
char2keypress 'Z'  = return $ (,) [ShiftModifier] ZKey

char2keypress '0'  = return $ (,) [     ] ZeroKey
char2keypress ')'  = return $ (,) [ShiftModifier] ZeroKey
char2keypress '1'  = return $ (,) [     ] OneKey
char2keypress '!'  = return $ (,) [ShiftModifier] OneKey
char2keypress '2'  = return $ (,) [     ] TwoKey
char2keypress '@'  = return $ (,) [ShiftModifier] TwoKey
char2keypress '3'  = return $ (,) [     ] ThreeKey
char2keypress '#'  = return $ (,) [ShiftModifier] ThreeKey
char2keypress '4'  = return $ (,) [     ] FourKey
char2keypress '$'  = return $ (,) [ShiftModifier] FourKey
char2keypress '5'  = return $ (,) [     ] FiveKey
char2keypress '%'  = return $ (,) [ShiftModifier] FiveKey
char2keypress '6'  = return $ (,) [     ] SixKey
char2keypress '^'  = return $ (,) [ShiftModifier] SixKey
char2keypress '7'  = return $ (,) [     ] SevenKey
char2keypress '&'  = return $ (,) [ShiftModifier] SevenKey
char2keypress '8'  = return $ (,) [     ] EightKey
char2keypress '*'  = return $ (,) [ShiftModifier] EightKey
char2keypress '9'  = return $ (,) [     ] NineKey
char2keypress '('  = return $ (,) [ShiftModifier] NineKey

char2keypress '`'  = return $ (,) [     ] GraveKey
char2keypress '~'  = return $ (,) [ShiftModifier] GraveKey
char2keypress '-'  = return $ (,) [     ] MinusKey
char2keypress '_'  = return $ (,) [ShiftModifier] MinusKey
char2keypress '='  = return $ (,) [     ] EqualKey
char2keypress '+'  = return $ (,) [ShiftModifier] EqualKey
char2keypress '['  = return $ (,) [     ] LeftBracketKey
char2keypress '{'  = return $ (,) [ShiftModifier] LeftBracketKey
char2keypress ']'  = return $ (,) [     ] RightBracketKey
char2keypress '}'  = return $ (,) [ShiftModifier] RightBracketKey
char2keypress '\\' = return $ (,) [     ] BackslashKey
char2keypress '|'  = return $ (,) [ShiftModifier] BackslashKey
char2keypress ';'  = return $ (,) [     ] SemicolonKey
char2keypress ':'  = return $ (,) [ShiftModifier] SemicolonKey
char2keypress '\'' = return $ (,) [     ] QuoteKey
char2keypress '"'  = return $ (,) [ShiftModifier] QuoteKey
char2keypress ','  = return $ (,) [     ] CommaKey
char2keypress '<'  = return $ (,) [ShiftModifier] CommaKey
char2keypress '.'  = return $ (,) [     ] PeriodKey
char2keypress '>'  = return $ (,) [ShiftModifier] PeriodKey
char2keypress '/'  = return $ (,) [     ] SlashKey
char2keypress '?'  = return $ (,) [ShiftModifier] SlashKey

char2keypress ' '  = return $ (,) [     ] SpaceKey
char2keypress '\t' = return $ (,) [     ] TabKey
char2keypress '\n' = return $ (,) [     ] ReturnKey

char2keypress c = failed $ "char2keypress: un-pressable Char: " <> show c


{- | the character that represents the keypress:

>>> keypress2char ([ShiftModifier], TwoKey) :: Maybe Char
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
keypress2char ((,) [ShiftModifier] AKey)            = return $ 'A'
keypress2char ((,) [     ] BKey)            = return $ 'b'
keypress2char ((,) [ShiftModifier] BKey)            = return $ 'B'
keypress2char ((,) [     ] CKey)            = return $ 'c'
keypress2char ((,) [ShiftModifier] CKey)            = return $ 'C'
keypress2char ((,) [     ] DKey)            = return $ 'd'
keypress2char ((,) [ShiftModifier] DKey)            = return $ 'D'
keypress2char ((,) [     ] EKey)            = return $ 'e'
keypress2char ((,) [ShiftModifier] EKey)            = return $ 'E'
keypress2char ((,) [     ] FKey)            = return $ 'f'
keypress2char ((,) [ShiftModifier] FKey)            = return $ 'F'
keypress2char ((,) [     ] GKey)            = return $ 'g'
keypress2char ((,) [ShiftModifier] GKey)            = return $ 'G'
keypress2char ((,) [     ] HKey)            = return $ 'h'
keypress2char ((,) [ShiftModifier] HKey)            = return $ 'H'
keypress2char ((,) [     ] IKey)            = return $ 'i'
keypress2char ((,) [ShiftModifier] IKey)            = return $ 'I'
keypress2char ((,) [     ] JKey)            = return $ 'j'
keypress2char ((,) [ShiftModifier] JKey)            = return $ 'J'
keypress2char ((,) [     ] KKey)            = return $ 'k'
keypress2char ((,) [ShiftModifier] KKey)            = return $ 'K'
keypress2char ((,) [     ] LKey)            = return $ 'l'
keypress2char ((,) [ShiftModifier] LKey)            = return $ 'L'
keypress2char ((,) [     ] MKey)            = return $ 'm'
keypress2char ((,) [ShiftModifier] MKey)            = return $ 'M'
keypress2char ((,) [     ] NKey)            = return $ 'n'
keypress2char ((,) [ShiftModifier] NKey)            = return $ 'N'
keypress2char ((,) [     ] OKey)            = return $ 'o'
keypress2char ((,) [ShiftModifier] OKey)            = return $ 'O'
keypress2char ((,) [     ] PKey)            = return $ 'p'
keypress2char ((,) [ShiftModifier] PKey)            = return $ 'P'
keypress2char ((,) [     ] QKey)            = return $ 'q'
keypress2char ((,) [ShiftModifier] QKey)            = return $ 'Q'
keypress2char ((,) [     ] RKey)            = return $ 'r'
keypress2char ((,) [ShiftModifier] RKey)            = return $ 'R'
keypress2char ((,) [     ] SKey)            = return $ 's'
keypress2char ((,) [ShiftModifier] SKey)            = return $ 'S'
keypress2char ((,) [     ] TKey)            = return $ 't'
keypress2char ((,) [ShiftModifier] TKey)            = return $ 'T'
keypress2char ((,) [     ] UKey)            = return $ 'u'
keypress2char ((,) [ShiftModifier] UKey)            = return $ 'U'
keypress2char ((,) [     ] VKey)            = return $ 'v'
keypress2char ((,) [ShiftModifier] VKey)            = return $ 'V'
keypress2char ((,) [     ] WKey)            = return $ 'w'
keypress2char ((,) [ShiftModifier] WKey)            = return $ 'W'
keypress2char ((,) [     ] XKey)            = return $ 'x'
keypress2char ((,) [ShiftModifier] XKey)            = return $ 'X'
keypress2char ((,) [     ] YKey)            = return $ 'y'
keypress2char ((,) [ShiftModifier] YKey)            = return $ 'Y'
keypress2char ((,) [     ] ZKey)            = return $ 'z'
keypress2char ((,) [ShiftModifier] ZKey)            = return $ 'Z'

keypress2char ((,) [     ] ZeroKey)         = return $ '0'
keypress2char ((,) [ShiftModifier] ZeroKey)         = return $ ')'
keypress2char ((,) [     ] OneKey)          = return $ '1'
keypress2char ((,) [ShiftModifier] OneKey)          = return $ '!'
keypress2char ((,) [     ] TwoKey)          = return $ '2'
keypress2char ((,) [ShiftModifier] TwoKey)          = return $ '@'
keypress2char ((,) [     ] ThreeKey)        = return $ '3'
keypress2char ((,) [ShiftModifier] ThreeKey)        = return $ '#'
keypress2char ((,) [     ] FourKey)         = return $ '4'
keypress2char ((,) [ShiftModifier] FourKey)         = return $ '$'
keypress2char ((,) [     ] FiveKey)         = return $ '5'
keypress2char ((,) [ShiftModifier] FiveKey)         = return $ '%'
keypress2char ((,) [     ] SixKey)          = return $ '6'
keypress2char ((,) [ShiftModifier] SixKey)          = return $ '^'
keypress2char ((,) [     ] SevenKey)        = return $ '7'
keypress2char ((,) [ShiftModifier] SevenKey)        = return $ '&'
keypress2char ((,) [     ] EightKey)        = return $ '8'
keypress2char ((,) [ShiftModifier] EightKey)        = return $ '*'
keypress2char ((,) [     ] NineKey)         = return $ '9'
keypress2char ((,) [ShiftModifier] NineKey)         = return $ '('

keypress2char ((,) [     ] GraveKey)        = return $ '`'
keypress2char ((,) [ShiftModifier] GraveKey)        = return $ '~'
keypress2char ((,) [     ] MinusKey)        = return $ '-'
keypress2char ((,) [ShiftModifier] MinusKey)        = return $ '_'
keypress2char ((,) [     ] EqualKey)        = return $ '='
keypress2char ((,) [ShiftModifier] EqualKey)        = return $ '+'
keypress2char ((,) [     ] LeftBracketKey)  = return $ '['
keypress2char ((,) [ShiftModifier] LeftBracketKey)  = return $ '{'
keypress2char ((,) [     ] RightBracketKey) = return $ ']'
keypress2char ((,) [ShiftModifier] RightBracketKey) = return $ '}'
keypress2char ((,) [     ] BackslashKey)    = return $ '\\'
keypress2char ((,) [ShiftModifier] BackslashKey)    = return $ '|'
keypress2char ((,) [     ] SemicolonKey)    = return $ ';'
keypress2char ((,) [ShiftModifier] SemicolonKey)    = return $ ':'
keypress2char ((,) [     ] QuoteKey)        = return $ '\''
keypress2char ((,) [ShiftModifier] QuoteKey)        = return $ '"'
keypress2char ((,) [     ] CommaKey)        = return $ ','
keypress2char ((,) [ShiftModifier] CommaKey)        = return $ '<'
keypress2char ((,) [     ] PeriodKey)       = return $ '.'
keypress2char ((,) [ShiftModifier] PeriodKey)       = return $ '>'
keypress2char ((,) [     ] SlashKey)        = return $ '/'
keypress2char ((,) [ShiftModifier] SlashKey)        = return $ '?'

keypress2char ((,) [     ] SpaceKey)        = return $ ' '
keypress2char ((,) [     ] TabKey)          = return $ '\t'
keypress2char ((,) [     ] ReturnKey)       = return $ '\n'

keypress2char k = failed $ "keypress2char: non-unicode-representable Keypress: " <> show k

-- TODO partial isomorphism between char2keypress and keypress2char?
