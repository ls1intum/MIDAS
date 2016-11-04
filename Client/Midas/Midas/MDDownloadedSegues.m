//
//  MDDownloadedSegues.m
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedSegues.h"

#import "MDRecordedSegue.h"

@implementation MDDownloadedSegues
@dynamic allRecordings;

-(id)handleMsgpack:(NSDictionary *)dict {
	return [[MDRecordedSegue alloc] initFromDictionary:dict];
}

-(NSURL *)downloadURL {
	NSURL *url = [NSURL URLWithString:@"segues.msgpack" relativeToURL:[self.class deviceSpecificBaseURL]];
	return url;
}

@end
