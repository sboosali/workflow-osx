#import <Foundation/Foundation.h>
#import "Recognizer.h"

void PrintRecognition(const char* s) {
    printf("[RECOGNIZED] %s\n", s);
}

void main2() {
    NSRunLoop *nsloop = [NSRunLoop currentRunLoop];
    CFRunLoopRef cfloop = [nsloop getCFRunLoop];
    CFArrayRef cfmodes = CFRunLoopCopyAllModes(cfloop);
    NSArray* nsmodes = CFBridgingRelease(cfmodes); // we've disabled ARC anyway
    
    for (id _mode in nsmodes) {
        CFStringRef cfmode = (CFStringRef)_mode;
        NSString* nsmode = CFBridgingRelease(cfmode);
        NSLog(@"%@", nsmode); // kCFRunLoopDefaultMode
    }
}

//NOTE concurrently: command&control overrides dictation, dictation can stil be recognized.

// @autoreleasepool {}
//    [r.recognizer setCommands:@[@"stop listening",@"start listening"]];

int main(int argc, const char * argv[]) {
    NSLog(@"----------------");
    
    const int length = 2;
    const char* commands[length];
    commands[0] = "stop listening";
    commands[1] = "start listening";
    
    Recognizer* r = [Recognizer new];
    setHandler_NSSpeechRecognizer(r, PrintRecognition);
    setCommands_NSSpeechRecognizer(r, commands, length);
    setExclusivity_NSSpeechRecognizer(r, YES);
    setForegroundOnly_NSSpeechRecognizer(r, NO);
    start_NSSpeechRecognizer(r);
    
    beginRunLoop();
    
    return 0;
}

