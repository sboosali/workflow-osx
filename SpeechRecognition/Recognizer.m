#import <Foundation/Foundation.h>
@import AppKit;

#import "Recognizer.h"

////////////////////////////////////////////////////////////////////////////////

/*
 
NSArray<NSString*>*_  ~  char*_[]
 
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

//void fromUTF8(const char* c, NSString* ns) {
//    ns = [NSString stringWithCString:c encoding:NSUTF8StringEncoding];
//}
//
//const char* toUTF8(NSString* s) {
//    return [s cStringUsingEncoding:NSUTF8StringEncoding];
//}
//
//int length(void* a[]) {
//    int i = 0;
//    while (a[i] != NULL) {
//        i++;
//    }
//    return i;
//}

/*
 input: c_array_of_c_strings, length
 output: ns_array_of_ns_strings
 
 you own the output
 */
NSArray<NSString*>* c2NSStringArray(const char* c_array_of_c_strings[], int length)
{
    NSArray<NSString*>* ns_array_of_ns_strings;
    NSString*            c_array_of_ns_strings[length];
    
    NSString*  ns_string;
    const char* c_string;
    
    for (int i = 0; i < length; i++) {
        c_string = c_array_of_c_strings[i];
        ns_string = [NSString stringWithCString:c_string encoding:NSUTF8StringEncoding];
        c_array_of_ns_strings[i] = ns_string;
    }
    
    ns_array_of_ns_strings = [NSArray arrayWithObjects:c_array_of_ns_strings count:(NSUInteger)length];
    return ns_array_of_ns_strings;
}

void beginRunLoop() {
    [[NSRunLoop currentRunLoop] run];
}

////////////////////////////////////////////////////////////////////////////////

/*
 
 self.recognizer.delegate                = self;
 self.handler = 0;
 - (void) speechRecognizer:... didRecognizeCommand:... {
     self.handler(recognition);
 }
 
 */
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

void setExclusivity_NSSpeechRecognizer(Recognizer* r, BOOL b) {
    r.recognizer.blocksOtherRecognizers = b;
}

void setForegroundOnly_NSSpeechRecognizer(Recognizer* r, BOOL b) {
    r.recognizer.listensInForegroundOnly = b;
}

// {{ const char* _[] }} is an array of strings
// {{ void* a[l] }} is a "variable length arrays". its lifetime is the block's.
void setCommands_NSSpeechRecognizer(Recognizer* this, const char* _commands[], int length){
    NSArray<NSString*>* commands = c2NSStringArray(_commands, length);
    
    this.recognizer.commands = commands;
}

void setHandler_NSSpeechRecognizer(Recognizer* this, void(*handler)(const char*)){
    this.handler = handler;
}

////////////////////////////////////////////////////////////////////////////////