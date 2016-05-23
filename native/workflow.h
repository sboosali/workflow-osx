#ifndef CBITS_OBJC_ACTOR_H
#define CBITS_OBJC_ACTOR_H 1
// why the guard? whats this macro?

#import <Cocoa/Cocoa.h>

ProcessSerialNumber* getApplicationPSN(const char* s);

void pressKey(CGEventFlags modifiers, CGKeyCode key);
void pressKeyToCurrentApplication(CGEventFlags modifiers, CGKeyCode key);
void pressKeyTo(CGEventFlags modifiers, CGKeyCode key, ProcessSerialNumber psn);
void clickMouse(CGEventFlags modifiers, CGMouseButton mouseButton, CGEventType mouseDown, CGEventType mouseUp, UInt32 numClicks);
const char* getClipboard();
void setClipboard(const char* contents);
const char * currentApplicationPath();
void openApplication(const char* s);
void openURL(const char* url);
void sendUnichar (unichar c);

#endif
