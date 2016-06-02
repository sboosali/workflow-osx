#import <Foundation/Foundation.h>
@import AppKit;

#import "Recognizer.h"

////////////////////////////////////////////////////////////////////////////////

/*
 
 "NSArray<NSString*>*" "char*[]"
 
 */
//NSArray<NSString*>* toNSStringArray(const char* _strings[]) { //TODO in haskell
//    NSArray<NSString*>* strings = _strings;
//    return strings;
//}

//const char** fromNSStringArray(NSArray<NSString*>* _strings) {
//    const char** strings = _strings;
//    return strings;
//}

//NSString* fromUTF8(const char* s) {
//    return [NSString stringWithCString:s encoding:NSUTF8StringEncoding];
//}

void fromUTF8(const char* c, NSString* ns) {
    ns = [NSString stringWithCString:c encoding:NSUTF8StringEncoding];
}

const char* toUTF8(NSString* s) {
    return [s cStringUsingEncoding:NSUTF8StringEncoding];
}

////////////////////////////////////////////////////////////////////////////////

@implementation Recognizer

- (id) init {
    if (self = [super init]) {
        self.recognizer = [NSSpeechRecognizer new];
        self.recognizer.listensInForegroundOnly = NO;
        self.recognizer.blocksOtherRecognizers  = NO;
        self.recognizer.delegate                = self;
        self.recognizer.commands                = @[];
        
        self.handler = 0; //NOTE unsafe
    }
    
    return self;
}

//
- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString*) _recognition {
//    NSLog(@"RECOGNIZED: %@", _recognition);
    const char* recognition = [_recognition cStringUsingEncoding:NSUTF8StringEncoding];
    self.handler(recognition);
}

@end

////////////////////////////////////////////////////////////////////////////////
// self is a keyword

Recognizer* new_NSSpeechRecognizer(){
    return [Recognizer new];
}

void free_NSSpeechRecognizer(Recognizer* this) { //TODO
    [this.recognizer.commands release];
    [this.recognizer release];
    [this release];
}

void start_NSSpeechRecognizer(Recognizer* this) {
    [this.recognizer startListening];
}

void stop_NSSpeechRecognizer(Recognizer* this) {
    [this.recognizer stopListening];
}

void setCommands_NSSpeechRecognizer(Recognizer* this, const char* givenCommands[]){
//    NSArray<NSString*>* previousCommands = this.recognizer.commands;
//    
//    NSArray<NSString*>* currentCommands = toNSStringArray(givenCommands);
//    this.recognizer.commands = currentCommands;
//
//    [previousCommands release]; // the block isn't atomic, we must save and free.
}

void setHandler_NSSpeechRecognizer(Recognizer* this, void(*handler)(const char*)){
    this.handler = handler;
}

////////////////////////////////////////////////////////////////////////////////