#import <Foundation/Foundation.h>
@import AppKit;

#import "Recognizer.h"

////////////////////////////////////////////////////////////////////////////////

@implementation Recognizer

- (id) init {
    if (self = [super init]) {
        self.recognizer = [NSSpeechRecognizer new];
        self.recognizer.listensInForegroundOnly = NO;
        self.recognizer.blocksOtherRecognizers  = YES;
        self.recognizer.delegate                = self;
        self.recognizer.commands                = @[];
        
        self.callback = 0;
    }
    
    return self;
}

- (void) setCommands:(NSArray<NSString*>*)commands {
    self.recognizer.commands = commands;
}

- (NSArray<NSString*>*) getCommands {
    return self.recognizer.commands;
}

- (void) start {
    [self.recognizer startListening];
}


- (void) stop {
    [self.recognizer stopListening];
}

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString*) recognition {
    NSLog(@"RECOGNIZED: %@", recognition);
    self.callback(recognition);
}

- (void) registerCallback:(void*) callback {
    self.callback = callback;
}

@end

////////////////////////////////////////////////////////////////////////////////

/*
 
 "NSArray<NSString*>*" "char*[]"

 */
NSArray<NSString*>* toNSStringArray(const char* _strings[]) {
    NSArray<NSString*>* strings = _strings;
    return strings;
}

const char** fromNSStringArray(NSArray<NSString*>* _strings) {
    const char** strings = _strings;
    return strings;
}


////////////////////////////////////////////////////////////////////////////////

Recognizer* new_NSSpeechRecognizer(){
    return [Recognizer new];
}

void free_NSSpeechRecognizer(Recognizer* this) {
    [this release];
}

void setCommands_NSSpeechRecognizer(Recognizer* this, const char* _vocabulary[]){
    NSArray<NSString*>* vocabulary = toNSStringArray(_vocabulary);
    [this setCommands:vocabulary];
//    [vocabulary release]; does setCommands copy it?
}

void start_NSSpeechRecognizer(Recognizer* this) {
    [this start];
}

void stop_NSSpeechRecognizer(Recognizer* this) {
    [this stop];
}


