#import <Foundation/Foundation.h>
#import "Recognizer.h"

void PrintRecognition(const char* s) {
    printf("[RECOGNIZED] %s\n", s);
}

//NOTE concurrently: command&control overrides dictation, dictation can stil be recognized.

// @autoreleasepool {}
//    [r.recognizer setCommands:@[@"stop listening",@"start listening"]];

int main(int argc, const char * argv[]) {
    
    const int length = 2;
    const char* commands[length];
    commands[0] = "stop listening";
    commands[1] = "start listening";
    
    Recognizer* r = [Recognizer new];
    setHandler_NSSpeechRecognizer(r, PrintRecognition);
    setCommands_NSSpeechRecognizer(r, commands, length);
    start_NSSpeechRecognizer(r);
    
    NSRunLoop *loop = [NSRunLoop currentRunLoop];
    [loop run];
    return 0;
}

