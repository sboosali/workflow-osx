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

@property (retain, readonly) NSSpeechRecognizer* recognizer; // TODO nonatomic?
@property void* callback; // void(NSString*)

- (id) init;
- (void) setCommands:(NSArray<NSString*>*)commands;
- (NSArray<NSString*>*) setCommands;

- (void) start;
- (void) stop;

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString *)_recognition;
- (void) registerCallback:(void*)callback;

@end

////////////////////////////////////////////////////////////////////////////////

Recognizer* new_NSSpeechRecognizer();

void free_NSSpeechRecognizer(Recognizer*);

void setCommands_NSSpeechRecognizer(Recognizer*, const char* []);

void start_NSSpeechRecognizer(Recognizer*);

void stop_NSSpeechRecognizer(Recognizer*);

//void _NSSpeechRecognizer();

