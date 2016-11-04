//
//  MDDownloadedTouches.m
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedTouches.h"

#import "MDRecordedGestures.h"
#import "MDGesture.h"
#import "MDTouchSequence.h"


@interface MDDownloadedTouches()

@property(readwrite,nonatomic,strong) NSString *viewIdentifier;
@property(readwrite,nonatomic,strong) NSArray *original;

@end

@implementation MDDownloadedTouches
@dynamic allRecordings;

- (instancetype)initForView:(NSString *)viewIdentifier {
	self = [super init];
	if (self) {
		_viewIdentifier = viewIdentifier;
	}
	return self;
}

-(NSURL *)downloadURL {
	NSString *sub = [NSString stringWithFormat:@"%@/touches.msgpack",_viewIdentifier];
	NSURL *url = [NSURL URLWithString:sub relativeToURL:[self.class deviceSpecificBaseURL]];
	return url;
}

-(void)postprocess {
	_original = self.allRecordings;
	NSMutableArray *t = [[NSMutableArray alloc] init];
	for (MDRecordedGestures *gest in self.allRecordings) {
		for (MDGesture *g in gest.gestures) {
			[t addObjectsFromArray:g.touchSequences];
		}
	}
	
	self.allRecordings = t;
	dispatch_async(dispatch_get_main_queue(), ^{
		[self.delegate recordings:self.allRecordings didDownload:self];
	});
	
}

-(id)handleMsgpack:(NSDictionary *)dict {
	return [[MDRecordedGestures alloc] initFromDictionary:dict];
}


@end
