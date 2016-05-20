#import <Foundation/Foundation.h>
@import AppKit;

#import "VRecognizer.h"


@implementation VRecognizer
    
    - (id)initWithCommands:(NSArray<NSString*>*)commands {
        
        if (self = [super init]) {
            NSSpeechRecognizer* recognizer;
            recognizer = [NSSpeechRecognizer new];
            recognizer.listensInForegroundOnly = NO;
            recognizer.blocksOtherRecognizers = YES;
            recognizer.delegate = self;
            
            recognizer.commands = commands;
            
            [recognizer startListening];
        }
        
        return self;
    }
    
    
    - (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString*) recognition {
        // NSString* recognition = (NSString*) _recognition;
        NSLog(@"RECOGNIZED: %@", recognition);
        // POST(recognition);
    }

@end

