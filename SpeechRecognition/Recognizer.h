#ifndef speech_h
#define speech_h
#endif /* speech_h */

#import <Foundation/Foundation.h>
@import AppKit;

@interface Recognizer : NSObject <NSSpeechRecognizerDelegate>

@property NSSpeechRecognizer* recognizer;

- (id) init;
- (void) setCommands:(NSArray<NSString*>*)commands;

- (void) start;
- (void) stop;

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString *)_recognition;

@end

