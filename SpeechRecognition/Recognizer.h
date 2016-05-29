#ifndef speech_h
#define speech_h
#endif /* speech_h */

#import <Foundation/Foundation.h>
@import AppKit;

////////////////////////////////////////////////////////////////////////////////

@interface Recognizer : NSObject <NSSpeechRecognizerDelegate>

@property (retain) NSSpeechRecognizer* recognizer; // TODO nonatomic?

- (id) init;
- (void) setCommands:(NSArray<NSString*>*)commands;

- (void) start;
- (void) stop;

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString *)_recognition;

@end

////////////////////////////////////////////////////////////////////////////////

Recognizer* new_NSSpeechRecognizer();

void free_NSSpeechRecognizer(Recognizer*);

void setCommands_NSSpeechRecognizer(Recognizer*, const char* []);

void start_NSSpeechRecognizer(Recognizer*);

void stop_NSSpeechRecognizer(Recognizer*);

//void _NSSpeechRecognizer();

