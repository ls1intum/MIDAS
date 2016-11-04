//
//  MDDownloadedActions.m
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedActions.h"

#import "MDRecordedAction.h"


@interface MDDownloadedActions()

@property(readwrite,nonatomic,strong) NSString *viewIdentifier;

@end

@implementation MDDownloadedActions
@dynamic allRecordings;

- (instancetype)initForView:(NSString *)viewIdentifier {
	self = [super init];
	if (self) {
		_viewIdentifier = viewIdentifier;
	}
	return self;
}

-(NSURL *)downloadURL {
	NSString *sub = [NSString stringWithFormat:@"%@/actions.msgpack",_viewIdentifier];
	NSURL *url = [NSURL URLWithString:sub relativeToURL:[self.class deviceSpecificBaseURL]];
	return url;
}

-(id)handleMsgpack:(NSDictionary *)dict {
	return [[MDRecordedAction alloc] initFromDictionary:dict];
}

@end

