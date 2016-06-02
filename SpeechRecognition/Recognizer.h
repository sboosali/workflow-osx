#ifndef speech_h
#define speech_h
#endif /* speech_h */

#import <Foundation/Foundation.h>
@import AppKit;

// TODO
// 2 layers of callbacks (Delegate => self.callback)
// 3 layers of access (c wrappers => Recognizer methods => NSSpeechRecognizer methods)

////////////////////////////////////////////////////////////////////////////////

// CallbackRecognizer, only fields

@interface Recognizer : NSObject <NSSpeechRecognizerDelegate>

@property (retain, atomic) NSSpeechRecognizer* recognizer;
@property void(*handler)(const char*); // void(NSString*)

- (id) init;

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString *)_recognition;

@end

////////////////////////////////////////////////////////////////////////////////

Recognizer* new_NSSpeechRecognizer();

void free_NSSpeechRecognizer(Recognizer*);

void start_NSSpeechRecognizer(Recognizer*);

void stop_NSSpeechRecognizer(Recognizer*);

void setCommands_NSSpeechRecognizer(Recognizer*, const char* []);

void setHandler_NSSpeechRecognizer(Recognizer* this, void(*handler)(const char*));
