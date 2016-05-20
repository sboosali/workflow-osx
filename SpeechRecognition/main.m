#import <Foundation/Foundation.h>
#import "Recognizer.h"

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
     
     //Recognizer* recognizer = [[Recognizer alloc] initWithCommands:@[@"stop listening",@"start listening"]];
     
     Recognizer* recognizer = [Recognizer new];
     [recognizer setCommands:@[@"stop listening",@"start listening"]];
     [recognizer start];
     
    }
    
    NSRunLoop *loop = [NSRunLoop currentRunLoop];
    while (1) {
        [loop run];
    }


    return 0;
}
