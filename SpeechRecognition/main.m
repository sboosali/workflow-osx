#import <Foundation/Foundation.h>
#import "Recognizer.h"

//int main(int argc, const char * argv[]) {
//    @autoreleasepool {
//        // insert code here...
//        NSLog(@"Hello, World!");
//    }
//    return 0;
//}

void PrintRecognition(const char* s) {
    printf("[RECOGNIZED] %s\n", s);
}

int main(int argc, const char * argv[]) {
// @autoreleasepool {

    NSLog(@"-------------------");
    
    Recognizer* r = [Recognizer new];
    r.handler = PrintRecognition;
    [r.recognizer setCommands:@[@"stop listening",@"start listening"]];
    [r.recognizer startListening];
     
//    }
    
    NSRunLoop *loop = [NSRunLoop currentRunLoop];
    while (1) {
        [loop run];
    }


    return 0;
}

