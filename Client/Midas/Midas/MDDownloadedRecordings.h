//
//  MDDownloadedRecordings.h
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "MDDownloadedRecordingsDelegate.h"

@interface MDDownloadedRecordings : NSObject

-(void)load;

+(NSURL*)deviceSpecificBaseURL;
+(NSURL*)baseURL;

// for subclasses
-(id)handleMsgpack:(NSDictionary*)dict;
-(void)postprocess;
-(NSURL*)downloadURL;

// props
@property(readwrite,nonatomic) NSArray *allRecordings;
@property(readwrite,nonatomic,weak) id<MDDownloadedRecordingsDelegate> delegate;
@end
