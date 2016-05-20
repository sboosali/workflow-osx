#import <Foundation/Foundation.h>
#import "VRecognizer.h"

//int main(int argc, const char * argv[]) {
//    @autoreleasepool {
//        // insert code here...
//        NSLog(@"Hello, World!");
//    }
//    return 0;
//}


int main(int argc, const char * argv[]) {
 @autoreleasepool {

     NSLog(@"-------------------");
     
     VRecognizer* recognizer = [[VRecognizer alloc] initWithCommands:@[@"stop listening",@"start listening"]];
     
     // NSRunLoop* myRunLoop =
     [NSRunLoop currentRunLoop];
     
    }
    return 0;
}
