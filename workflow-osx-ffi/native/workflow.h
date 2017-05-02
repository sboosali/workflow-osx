#ifndef CBITS_OBJC_ACTOR_H
#define CBITS_OBJC_ACTOR_H 1
// TODO why the guard? whats this macro? bring endif up?

#import <Cocoa/Cocoa.h>

////////////////////////////////////////////////////////////////////////////////
// public

void sendUnichar (unichar c);

void pressKey     (CGEventFlags modifiers, CGKeyCode key);
void pressKeyDown (CGEventFlags modifiers, CGKeyCode key);
void pressKeyUp   (CGEventFlags modifiers, CGKeyCode key);

void pressKeyToCurrentApplication(CGEventFlags modifiers, CGKeyCode key);
void pressKeyTo(CGEventFlags modifiers, CGKeyCode key, ProcessSerialNumber psn);

void clickMouseAt
(CGEventFlags modifiers,
 UInt32 numClicks,
 CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp,
 CGFloat x, CGFloat y);

void clickMouse
(CGEventFlags modifiers, UInt32 numClicks, CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp);

const char* getClipboard();
void setClipboard(const char* contents);

const char * currentApplicationPath();
void openApplication(const char* s);

void openURL(const char* url);

void getCursorPosition(CGPoint* p);
void setCursorPosition(CGFloat x, CGFloat y);

////////////////////////////////////////////////////////////////////////////////

NSArray<NSDictionary*>* getApplications();
NSDictionary* getCurrentApplication ();

////////////////////////////////////////////////////////////////////////////////
// private

ProcessSerialNumber* getApplicationPSN(const char* s);

#endif
