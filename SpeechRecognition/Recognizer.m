#import <Foundation/Foundation.h>
@import AppKit;

#import "Recognizer.h"


@implementation Recognizer

- (id) init {
    if (self = [super init]) {
        self.recognizer = [NSSpeechRecognizer new];
        self.recognizer.listensInForegroundOnly = NO;
        self.recognizer.blocksOtherRecognizers  = YES;
        self.recognizer.delegate                = self;
        self.recognizer.commands                = @[];
    }
    
    return self;
}

- (void) setCommands:(NSArray<NSString*>*)commands {
    self.recognizer.commands = commands;
}

- (void) start {
    [self.recognizer startListening];
}


- (void) stop {
    [self.recognizer stopListening];
}

- (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString*) recognition {
        // NSString* recognition = (NSString*) _recognition;
        NSLog(@"RECOGNIZED: %@", recognition);
        // POST(recognition);
}

@end

