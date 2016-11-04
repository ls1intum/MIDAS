//
//  MDTouchSequence.m
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDTouchSequence.h"

#import "MDTouch.h"

#import "MDGesture.h"

@interface MDTouchSequence()

@property(readwrite,nonatomic) NSTimeInterval initialTimestamp;
//@property(readwrite,nonatomic,strong) NSMutableArray *touches;

@end

@implementation MDTouchSequence

- (instancetype)initFromDictionary:(NSDictionary *)dict {
	self = [super init];
	if (self) {
		_timeOffset = [[dict valueForKey:@"timeOffset"] doubleValue];
		_touches = [[NSMutableArray alloc] init];
		for (NSDictionary *d in [dict valueForKey:@"touches"]) {
			MDTouch *t = [[MDTouch alloc] initFromDictionary:d];
			if(t) {
				[_touches addObject:t];
			}
		}
	}
	return self;
}

-(instancetype)initWithTimeOffset:(NSTimeInterval)timeOffset {
	self = [super init];
	if (self) {
		_timeOffset = timeOffset;
		_initialTimestamp = [[NSProcessInfo processInfo] systemUptime]; // to get the relative touch-timestamp
		_touches = [NSMutableArray array];
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t data_size = sizeof(uint8_t) + sizeof(NSTimeInterval);
	uint8_t *data = malloc(data_size);
	
	data[0] = 0x30;
	
//	*((NSTimeInterval*)(&data[1])) = _timeOffset;
	memcpy(&data[1], &_timeOffset, sizeof(NSTimeInterval));
	
	NSLog(@"Writing Sequence: %.2f",_timeOffset);
	
	[encoder writeData:data ofLength:data_size];
	for (MDTouch *touch in _touches) {
		[encoder encodeObject:touch];
	}
}

-(BOOL)handleTouch:(UITouch *)touch {
	CGPoint location = [touch locationInView:nil];
	NSTimeInterval timeOffset = fmax(0.0, touch.timestamp - _initialTimestamp);


	
	MDTouch *newTouch = [[MDTouch alloc] initWithLocation:location timeOffset:timeOffset];
	[_touches addObject:newTouch];
	
	//NSLog(@"Sequence of %lu touches phase = %li gestures = %@",(unsigned long)_touches.count,(long)touch.phase,touch.gestureRecognizers);
	
	// NOT ended or cancelled means it should be kept alive
	return !(touch.phase == UITouchPhaseEnded || touch.phase == UITouchPhaseCancelled);
}


-(NSDictionary *)context:(NSString *)contextName {
	return [self.parent context:contextName];
}

@end
