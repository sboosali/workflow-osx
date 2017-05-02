{-# LANGUAGE PatternSynonyms #-}
{-| virtual key constants.

this module declares only `patterns`.

see:

* <https://developer.apple.com/library/mac/documentation/Carbon/Reference/QuartzEventServicesRef/>

* line 236 of
</System/Library/Frameworks/IOKit.framework/Versions/A/Headers/hidsystem/IOLLEvent.h>

* line 196 of
</System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>

@
/*
 *  Summary:
 *    Virtual keycodes
 *
 *  Discussion:
 *    These constants are the virtual keycodes defined originally in
 *    Inside Mac Volume V, pg. V-191. They identify physical keys on a
 *    keyboard. Those constants with \"ANSI\" in the name are labeled
 *    according to the key position on an ANSI-standard US keyboard.
 *    For example, kVK_ANSI_A indicates the virtual keycode for the key
 *    with the letter 'A' in the US keyboard layout. Other keyboard
 *    layouts may have the 'A' key label on a different physical key;
 *    in this case, pressing 'A' will generate a different virtual
 *    keycode.
 */
 @

-}
module Workflow.OSX.C.Constants where
import Workflow.OSX.C.Types (CGEventFlags, CGEventType, OSXMouseButton, CGMouseButton, CGKeyCode)

--------------------------------------------------------------------------------
-- Modifiers

-- device-independent

pattern NX_SHIFTMASK :: CGEventFlags
pattern NX_SHIFTMASK = 0x00020000

pattern NX_CONTROLMASK :: CGEventFlags
pattern NX_CONTROLMASK = 0x00040000

-- | @Option@ key
pattern NX_ALTERNATEMASK :: CGEventFlags
pattern NX_ALTERNATEMASK = 0x00080000

pattern NX_COMMANDMASK :: CGEventFlags
pattern NX_COMMANDMASK = 0x00100000

-- | @Fn@ key
pattern NX_SECONDARYFNMASK :: CGEventFlags
pattern NX_SECONDARYFNMASK = 0x00800000

-- yes: #define NX_CONTROLMASK 0x00040000
-- no: #define NX_DEVICELCTLKEYMASK 0x00000001
-- no: #define NX_DEVICERCTLKEYMASK 0x00002000

--------------------------------------------------------------------------------
-- Mouse

pattern OSXLeftButton   :: OSXMouseButton
pattern OSXLeftButton    = (CGMouseButtonLeft,  NX_LMOUSEDOWN,  NX_LMOUSEUP)

pattern OSXRightButton  :: OSXMouseButton
pattern OSXRightButton   = (CGMouseButtonRight, NX_RMOUSEDOWN,  NX_RMOUSEUP)

pattern OSXMiddleButton :: OSXMouseButton
pattern OSXMiddleButton  = (CGMouseButtonCenter, NX_OMOUSEDOWN, NX_OMOUSEUP) -- TODO "other" is "center"?

-- CGEventType

-- /* mouse events */

pattern NX_LMOUSEDOWN    :: CGEventType
pattern NX_LMOUSEDOWN     = 1  -- /* left mouse-down event */

pattern NX_LMOUSEUP    :: CGEventType
pattern NX_LMOUSEUP     = 2   -- /* left mouse-up event */

pattern NX_RMOUSEDOWN    :: CGEventType
pattern NX_RMOUSEDOWN     = 3  -- /* right mouse-down event */

pattern NX_RMOUSEUP    :: CGEventType
pattern NX_RMOUSEUP     =  4 -- /* right mouse-up event */

pattern NX_MOUSEMOVED    :: CGEventType
pattern NX_MOUSEMOVED     = 5  -- /* mouse-moved event */

pattern NX_LMOUSEDRAGGED    :: CGEventType
pattern NX_LMOUSEDRAGGED  = 6  -- /* left mouse-dragged event */

pattern NX_RMOUSEDRAGGED    :: CGEventType
pattern NX_RMOUSEDRAGGED  = 7 -- /*  right mouse-dragged event */

pattern NX_MOUSEENTERED    :: CGEventType
pattern NX_MOUSEENTERED   = 8 -- /* mouse-entered event */

pattern NX_MOUSEEXITED    :: CGEventType
pattern NX_MOUSEEXITED     = 9 -- /* mouse-exited event */

{-/* other mouse events
 *
 * event.data.mouse.buttonNumber should contain the
 * button number (2-31) changing state.
 */-}

pattern NX_OMOUSEDOWN    :: CGEventType
pattern NX_OMOUSEDOWN     = 25 -- /* other mouse-down event */

pattern NX_OMOUSEUP       :: CGEventType
pattern NX_OMOUSEUP       = 26 -- /* other mouse-up event */

pattern NX_OMOUSEDRAGGED :: CGEventType
pattern NX_OMOUSEDRAGGED  = 27 -- /* other mouse-dragged event */

-- CGMouseButton

pattern CGMouseButtonLeft   :: CGMouseButton
pattern CGMouseButtonLeft    = 0

pattern CGMouseButtonRight  :: CGMouseButton
pattern CGMouseButtonRight   = 1

pattern CGMouseButtonCenter :: CGMouseButton
pattern CGMouseButtonCenter  = 2

--------------------------------------------------------------------------------
-- Keys

{-

/* keyboard events */

#define NX_KEYDOWN		10	/* key-down event */
#define NX_KEYUP		11	/* key-up event */
#define NX_FLAGSCHANGED		12	/* flags-changed event */

@


sticky keys
@

// New stickykeys key events
// These were created to send an event describing the
// different state of the modifiers
#define NX_SUBTYPE_STICKYKEYS_SHIFT_DOWN		110
#define NX_SUBTYPE_STICKYKEYS_CONTROL_DOWN		111
#define NX_SUBTYPE_STICKYKEYS_ALTERNATE_DOWN		112
#define NX_SUBTYPE_STICKYKEYS_COMMAND_DOWN		113
#define NX_SUBTYPE_STICKYKEYS_FN_DOWN			114

#define NX_SUBTYPE_STICKYKEYS_SHIFT_LOCK		120
#define NX_SUBTYPE_STICKYKEYS_CONTROL_LOCK		121
#define NX_SUBTYPE_STICKYKEYS_ALTERNATE_LOCK		122
#define NX_SUBTYPE_STICKYKEYS_COMMAND_LOCK		123
#define NX_SUBTYPE_STICKYKEYS_FN_LOCK			124

#define NX_SUBTYPE_STICKYKEYS_SHIFT_UP			130
#define NX_SUBTYPE_STICKYKEYS_CONTROL_UP		131
#define NX_SUBTYPE_STICKYKEYS_ALTERNATE_UP		132
#define NX_SUBTYPE_STICKYKEYS_COMMAND_UP		133
#define NX_SUBTYPE_STICKYKEYS_FN_UP			134
@


slow keys
@

// SlowKeys
#define NX_SUBTYPE_SLOWKEYS_START			200
#define NX_SUBTYPE_SLOWKEYS_ABORT			201
#define NX_SUBTYPE_SLOWKEYS_END				202
@


mouse
line 82
@

/* mouse events */

#define NX_LMOUSEDOWN		1	/* left mouse-down event */
#define NX_LMOUSEUP		2	/* left mouse-up event */
#define NX_RMOUSEDOWN		3	/* right mouse-down event */
#define NX_RMOUSEUP		4	/* right mouse-up event */
#define NX_MOUSEMOVED		5	/* mouse-moved event */
#define NX_LMOUSEDRAGGED	6	/* left mouse-dragged event */
#define NX_RMOUSEDRAGGED	7	/* right mouse-dragged event */
#define NX_MOUSEENTERED		8	/* mouse-entered event */
#define NX_MOUSEEXITED		9	/* mouse-exited event */

/* other mouse events
 *
 * event.data.mouse.buttonNumber should contain the
 * button number (2-31) changing state.
 */
#define NX_OMOUSEDOWN		25	/* other mouse-down event */
#define NX_OMOUSEUP		26	/* other mouse-up event */
#define NX_OMOUSEDRAGGED	27	/* other mouse-dragged event */


/* Scroll wheel events */

#define NX_SCROLLWHEELMOVED	22

/* Zoom events */
#define NX_ZOOM             28
@

-}

--- "ANSI" means: keycodes for keys on a standard us keyboard layout

pattern VK_ANSI_A               :: CGKeyCode
pattern VK_ANSI_A                    = 0x00

pattern VK_ANSI_S               :: CGKeyCode
pattern VK_ANSI_S                    = 0x01

pattern VK_ANSI_D               :: CGKeyCode
pattern VK_ANSI_D                    = 0x02

pattern VK_ANSI_F               :: CGKeyCode
pattern VK_ANSI_F                    = 0x03

pattern VK_ANSI_H               :: CGKeyCode
pattern VK_ANSI_H                    = 0x04

pattern VK_ANSI_G               :: CGKeyCode
pattern VK_ANSI_G                    = 0x05

pattern VK_ANSI_Z               :: CGKeyCode
pattern VK_ANSI_Z                    = 0x06

pattern VK_ANSI_X               :: CGKeyCode
pattern VK_ANSI_X                    = 0x07

pattern VK_ANSI_C               :: CGKeyCode
pattern VK_ANSI_C                    = 0x08

pattern VK_ANSI_V               :: CGKeyCode
pattern VK_ANSI_V                    = 0x09

pattern VK_ANSI_B               :: CGKeyCode
pattern VK_ANSI_B                    = 0x0B

pattern VK_ANSI_Q               :: CGKeyCode
pattern VK_ANSI_Q                    = 0x0C

pattern VK_ANSI_W               :: CGKeyCode
pattern VK_ANSI_W                    = 0x0D

pattern VK_ANSI_E               :: CGKeyCode
pattern VK_ANSI_E                    = 0x0E

pattern VK_ANSI_R               :: CGKeyCode
pattern VK_ANSI_R                    = 0x0F

pattern VK_ANSI_Y               :: CGKeyCode
pattern VK_ANSI_Y                    = 0x10

pattern VK_ANSI_T               :: CGKeyCode
pattern VK_ANSI_T                    = 0x11

pattern VK_ANSI_1               :: CGKeyCode
pattern VK_ANSI_1                    = 0x12

pattern VK_ANSI_2               :: CGKeyCode
pattern VK_ANSI_2                    = 0x13

pattern VK_ANSI_3               :: CGKeyCode
pattern VK_ANSI_3                    = 0x14

pattern VK_ANSI_4               :: CGKeyCode
pattern VK_ANSI_4                    = 0x15

pattern VK_ANSI_6               :: CGKeyCode
pattern VK_ANSI_6                    = 0x16

pattern VK_ANSI_5               :: CGKeyCode
pattern VK_ANSI_5                    = 0x17

pattern VK_ANSI_Equal           :: CGKeyCode
pattern VK_ANSI_Equal                = 0x18

pattern VK_ANSI_9               :: CGKeyCode
pattern VK_ANSI_9                    = 0x19

pattern VK_ANSI_7               :: CGKeyCode
pattern VK_ANSI_7                    = 0x1A

pattern VK_ANSI_Minus           :: CGKeyCode
pattern VK_ANSI_Minus                = 0x1B

pattern VK_ANSI_8               :: CGKeyCode
pattern VK_ANSI_8                    = 0x1C

pattern VK_ANSI_0               :: CGKeyCode
pattern VK_ANSI_0                    = 0x1D

pattern VK_ANSI_RightBracket    :: CGKeyCode
pattern VK_ANSI_RightBracket         = 0x1E

pattern VK_ANSI_O               :: CGKeyCode
pattern VK_ANSI_O                    = 0x1F

pattern VK_ANSI_U               :: CGKeyCode
pattern VK_ANSI_U                    = 0x20

pattern VK_ANSI_LeftBracket     :: CGKeyCode
pattern VK_ANSI_LeftBracket          = 0x21

pattern VK_ANSI_I               :: CGKeyCode
pattern VK_ANSI_I                    = 0x22

pattern VK_ANSI_P               :: CGKeyCode
pattern VK_ANSI_P                    = 0x23

pattern VK_ANSI_L               :: CGKeyCode
pattern VK_ANSI_L                    = 0x25

pattern VK_ANSI_J               :: CGKeyCode
pattern VK_ANSI_J                    = 0x26

pattern VK_ANSI_Quote           :: CGKeyCode
pattern VK_ANSI_Quote                = 0x27

pattern VK_ANSI_K               :: CGKeyCode
pattern VK_ANSI_K                    = 0x28

pattern VK_ANSI_Semicolon       :: CGKeyCode
pattern VK_ANSI_Semicolon            = 0x29

pattern VK_ANSI_Backslash       :: CGKeyCode
pattern VK_ANSI_Backslash            = 0x2A

pattern VK_ANSI_Comma           :: CGKeyCode
pattern VK_ANSI_Comma                = 0x2B

pattern VK_ANSI_Slash           :: CGKeyCode
pattern VK_ANSI_Slash                = 0x2C

pattern VK_ANSI_N               :: CGKeyCode
pattern VK_ANSI_N                    = 0x2D

pattern VK_ANSI_M               :: CGKeyCode
pattern VK_ANSI_M                    = 0x2E

pattern VK_ANSI_Period          :: CGKeyCode
pattern VK_ANSI_Period               = 0x2F

pattern VK_ANSI_Grave           :: CGKeyCode
pattern VK_ANSI_Grave                = 0x32

pattern VK_ANSI_KeypadDecimal   :: CGKeyCode
pattern VK_ANSI_KeypadDecimal        = 0x41

pattern VK_ANSI_KeypadMultiply  :: CGKeyCode
pattern VK_ANSI_KeypadMultiply       = 0x43

pattern VK_ANSI_KeypadPlus      :: CGKeyCode
pattern VK_ANSI_KeypadPlus           = 0x45

pattern VK_ANSI_KeypadClear     :: CGKeyCode
pattern VK_ANSI_KeypadClear          = 0x47

pattern VK_ANSI_KeypadDivide    :: CGKeyCode
pattern VK_ANSI_KeypadDivide         = 0x4B

pattern VK_ANSI_KeypadEnter     :: CGKeyCode
pattern VK_ANSI_KeypadEnter          = 0x4C

pattern VK_ANSI_KeypadMinus     :: CGKeyCode
pattern VK_ANSI_KeypadMinus          = 0x4E

pattern VK_ANSI_KeypadEquals    :: CGKeyCode
pattern VK_ANSI_KeypadEquals         = 0x51

pattern VK_ANSI_Keypad0         :: CGKeyCode
pattern VK_ANSI_Keypad0              = 0x52

pattern VK_ANSI_Keypad1         :: CGKeyCode
pattern VK_ANSI_Keypad1              = 0x53

pattern VK_ANSI_Keypad2         :: CGKeyCode
pattern VK_ANSI_Keypad2              = 0x54

pattern VK_ANSI_Keypad3         :: CGKeyCode
pattern VK_ANSI_Keypad3              = 0x55

pattern VK_ANSI_Keypad4         :: CGKeyCode
pattern VK_ANSI_Keypad4              = 0x56

pattern VK_ANSI_Keypad5         :: CGKeyCode
pattern VK_ANSI_Keypad5              = 0x57

pattern VK_ANSI_Keypad6         :: CGKeyCode
pattern VK_ANSI_Keypad6              = 0x58

pattern VK_ANSI_Keypad7         :: CGKeyCode
pattern VK_ANSI_Keypad7              = 0x59

pattern VK_ANSI_Keypad8         :: CGKeyCode
pattern VK_ANSI_Keypad8              = 0x5B

pattern VK_ANSI_Keypad9         :: CGKeyCode
pattern VK_ANSI_Keypad9              = 0x5


--- "keycodes for keys that are independent of keyboard layout"

pattern VK_Return               :: CGKeyCode
pattern VK_Return                    = 0x24

pattern VK_Tab                  :: CGKeyCode
pattern VK_Tab                       = 0x30

pattern VK_Space                :: CGKeyCode
pattern VK_Space                     = 0x31

pattern VK_Delete               :: CGKeyCode
pattern VK_Delete                    = 0x33

pattern VK_Escape               :: CGKeyCode
pattern VK_Escape                    = 0x35

pattern VK_Command              :: CGKeyCode
pattern VK_Command                   = 0x37

pattern VK_Shift                :: CGKeyCode
pattern VK_Shift                     = 0x38

pattern VK_CapsLock             :: CGKeyCode
pattern VK_CapsLock                  = 0x39

pattern VK_Option               :: CGKeyCode
pattern VK_Option                    = 0x3A

pattern VK_Control              :: CGKeyCode
pattern VK_Control                   = 0x3B

pattern VK_RightShift           :: CGKeyCode
pattern VK_RightShift                = 0x3C

pattern VK_RightOption          :: CGKeyCode
pattern VK_RightOption               = 0x3D

pattern VK_RightControl         :: CGKeyCode
pattern VK_RightControl              = 0x3E

pattern VK_Function             :: CGKeyCode
pattern VK_Function                  = 0x3F

pattern VK_F17                  :: CGKeyCode
pattern VK_F17                       = 0x40

pattern VK_VolumeUp             :: CGKeyCode
pattern VK_VolumeUp                  = 0x48

pattern VK_VolumeDown           :: CGKeyCode
pattern VK_VolumeDown                = 0x49

pattern VK_Mute                 :: CGKeyCode
pattern VK_Mute                      = 0x4A

pattern VK_F18                  :: CGKeyCode
pattern VK_F18                       = 0x4F

pattern VK_F19                  :: CGKeyCode
pattern VK_F19                       = 0x50

pattern VK_F20                  :: CGKeyCode
pattern VK_F20                       = 0x5A

pattern VK_F5                   :: CGKeyCode
pattern VK_F5                        = 0x60

pattern VK_F6                   :: CGKeyCode
pattern VK_F6                        = 0x61

pattern VK_F7                   :: CGKeyCode
pattern VK_F7                        = 0x62

pattern VK_F3                   :: CGKeyCode
pattern VK_F3                        = 0x63

pattern VK_F8                   :: CGKeyCode
pattern VK_F8                        = 0x64

pattern VK_F9                   :: CGKeyCode
pattern VK_F9                        = 0x65

pattern VK_F11                  :: CGKeyCode
pattern VK_F11                       = 0x67

pattern VK_F13                  :: CGKeyCode
pattern VK_F13                       = 0x69

pattern VK_F16                  :: CGKeyCode
pattern VK_F16                       = 0x6A

pattern VK_F14                  :: CGKeyCode
pattern VK_F14                       = 0x6B

pattern VK_F10                  :: CGKeyCode
pattern VK_F10                       = 0x6D

pattern VK_F12                  :: CGKeyCode
pattern VK_F12                       = 0x6F

pattern VK_F15                  :: CGKeyCode
pattern VK_F15                       = 0x71

pattern VK_Help                 :: CGKeyCode
pattern VK_Help                      = 0x72

pattern VK_Home                 :: CGKeyCode
pattern VK_Home                      = 0x73

pattern VK_PageUp               :: CGKeyCode
pattern VK_PageUp                    = 0x74

pattern VK_ForwardDelete        :: CGKeyCode
pattern VK_ForwardDelete             = 0x75

pattern VK_F4                   :: CGKeyCode
pattern VK_F4                        = 0x76

pattern VK_End                  :: CGKeyCode
pattern VK_End                       = 0x77

pattern VK_F2                   :: CGKeyCode
pattern VK_F2                        = 0x78

pattern VK_PageDown             :: CGKeyCode
pattern VK_PageDown                  = 0x79

pattern VK_F1                   :: CGKeyCode
pattern VK_F1                        = 0x7A

pattern VK_LeftArrow            :: CGKeyCode
pattern VK_LeftArrow                 = 0x7B

pattern VK_RightArrow           :: CGKeyCode
pattern VK_RightArrow                = 0x7C

pattern VK_DownArrow            :: CGKeyCode
pattern VK_DownArrow                 = 0x7D

pattern VK_UpArrow              :: CGKeyCode
pattern VK_UpArrow                   = 0x7


--- "ISO keyboards only"

pattern VK_ISO_Section            :: CGKeyCode
pattern VK_ISO_Section               = 0x0


--- "JIS keyboards only"

pattern VK_JIS_Yen              :: CGKeyCode
pattern VK_JIS_Yen                   = 0x5D

pattern VK_JIS_Underscore       :: CGKeyCode
pattern VK_JIS_Underscore            = 0x5E

pattern VK_JIS_KeypadComma      :: CGKeyCode
pattern VK_JIS_KeypadComma           = 0x5F

pattern VK_JIS_Eisu             :: CGKeyCode
pattern VK_JIS_Eisu                  = 0x66

pattern VK_JIS_Kana             :: CGKeyCode
pattern VK_JIS_Kana                  = 0x6
