#ifndef CBITS_OBJC_ACTOR_H
#define CBITS_OBJC_ACTOR_H 1
// TODO why the guard? whats this macro? bring endif up?

#import <Cocoa/Cocoa.h>

////////////////////////////////////////////////////////////////////////////////

void sendUnichar (unichar c);

void pressKey(CGEventFlags modifiers, CGKeyCode key);
void pressKeyToCurrentApplication(CGEventFlags modifiers, CGKeyCode key);
void pressKeyTo(CGEventFlags modifiers, CGKeyCode key, ProcessSerialNumber psn);

void clickMouseAt
(CGEventFlags modifiers, UInt32 numClicks, CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp, CGPoint p);
void clickMouse
(CGEventFlags modifiers, UInt32 numClicks, CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp);

const char* getClipboard();
void setClipboard(const char* contents);

const char * currentApplicationPath();
void openApplication(const char* s);

void openURL(const char* url);

////////////////////////////////////////////////////////////////////////////////

void getCursorPosition (CGPoint*);
ProcessSerialNumber* getApplicationPSN(const char* s);

#endif
