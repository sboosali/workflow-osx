//
//  speech.h
//  SpeechRecognition
//
//  Created by Michael Boosalis on 5/20/16.
//  Copyright © 2016 Michael Boosalis. All rights reserved. Property Is Sacred.
//

#ifndef speech_h
#define speech_h


#endif /* speech_h */

#import <Foundation/Foundation.h>
@import AppKit;

@interface VRecognizer : NSObject <NSSpeechRecognizerDelegate>
    
    - (id) initWithCommands:(NSArray<NSString*>*)commands;
    
    - (void) speechRecognizer:(NSSpeechRecognizer *)recognizer didRecognizeCommand:(NSString *)_recognition;

@end

