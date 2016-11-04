//
//  MDGesture.m
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDGesture.h"

#import "MDRecordedGestures.h"

#import "MDTouchSequence.h"
#import "MDCoding.h"

#import "MDEncodeTypes.h"

#import "MDActivityContext.h"
#import "MDMotionContext.h"


typedef NS_OPTIONS(NSUInteger, MDInvolvedGesture) {
	MDInvolvedGestureTap = 1 << 0,
	MDInvolvedGesturePinch = 1 << 1,
	MDInvolvedGestureRotation = 1 << 2,
	MDInvolvedGestureSwipe = 1 << 3,
	MDInvolvedGesturePan = 1 << 4,
	MDInvolvedGestureScreenEdgePan = 1 << 5,
	MDInvolvedGestureLongPress = 1 << 6,
};


@interface MDGesture()<MDCoding>

@property(readwrite,nonatomic) NSTimeInterval initialTimestamp;

@property(readwrite,nonatomic,strong) NSMapTable *currentTouches;
@property(readwrite,nonatomic,strong) NSMutableArray *finishedTouches;


@property(readwrite,nonatomic) uint8_t gestureFlags;
@end

@implementation MDGesture

-(instancetype)initFromDictionary:(NSDictionary *)dict {
	self = [super init];
	if (self) {
		_timeOffset = [[dict valueForKey:@"timeOffset"] doubleValue];
		NSMutableArray *touchSequences = [NSMutableArray array];
		for (NSDictionary *d in [dict valueForKey:@"touchSequence"]) {
			MDTouchSequence *ts = [[MDTouchSequence alloc] initFromDictionary:d];
			if(ts) {
				ts.parent = self;
				[touchSequences addObject:ts];
			}
		}
		_touchSequences = touchSequences;
		
		NSMutableDictionary *ctxs = [NSMutableDictionary dictionary];
		for (NSString *k in dict.keyEnumerator) {
			if([k hasSuffix:@"_context"]) {
				[ctxs setValue:[dict valueForKey:k] forKey:k];
			}
		}
		_contexts = ctxs;
	}
	return self;
}

-(instancetype)initWithTimeOffset:(NSTimeInterval)timeOffset {
	self = [super init];
	if (self) {
		_gestureFlags = 0;
		_timeOffset = timeOffset;
		_initialTimestamp = [[NSProcessInfo processInfo] systemUptime]; // to get the relative touch-timestamp
		_currentTouches = [NSMapTable mapTableWithKeyOptions:NSPointerFunctionsStrongMemory valueOptions:NSPointerFunctionsStrongMemory];
		_finishedTouches = [NSMutableArray array];
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t data_size = (sizeof(uint8_t)*2) + sizeof(NSTimeInterval);
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Gesture;
	data[1] = _gestureFlags;

//	*((NSTimeInterval*)(&data[2])) = _timeOffset;
	memcpy(&data[2], &_timeOffset, sizeof(NSTimeInterval));

	
	[encoder writeData:data ofLength:data_size];
	[encoder encodeObject:[MDActivityContext currentContext]];
	[encoder encodeObject:[MDMotionContext currentContext]];
	for (MDTouchSequence *seq in _finishedTouches) {
		[encoder encodeObject:seq];
	}
	
}


-(BOOL)handleTouches:(NSSet<UITouch *> *)touches {
	[touches enumerateObjectsUsingBlock:^(UITouch *touch, BOOL *stop) {
//		NSLog(@"Touchphase %lu",touch.phase);
		MDTouchSequence *sequence = [_currentTouches objectForKey:touch];
		if(sequence == nil) {
			if((touch.phase == UITouchPhaseBegan || touch.phase == UITouchPhaseStationary) == NO) {
				NSLog(@"Warning: Starting a sequence with a touch phase other than began %@",touch);
			}
			sequence = [[MDTouchSequence alloc] initWithTimeOffset:fmax(0.0, touch.timestamp - _initialTimestamp)];
			[_currentTouches setObject:sequence forKey:touch];
		}
		
		if([sequence handleTouch:touch] == NO) {
			NSLog(@"Touch sequence ended.");
			[_finishedTouches addObject:sequence];
			[_currentTouches removeObjectForKey:touch];
			
			// if this is the last touch, get it's gesture recognizers and add them to the flag
			if(_currentTouches.count == 0) {
				uint8_t gestureFlags = 0;
				for(UIGestureRecognizer *recognizer in touch.gestureRecognizers) {
					if([recognizer isKindOfClass:[UITapGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGestureTap;
					} else if([recognizer isKindOfClass:[UIPinchGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGesturePinch;
					} else if([recognizer isKindOfClass:[UIRotationGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGestureRotation;
					} else if([recognizer isKindOfClass:[UISwipeGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGestureSwipe;
					} else if([recognizer isKindOfClass:[UIPanGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGesturePan;
					} else if([recognizer isKindOfClass:[UIScreenEdgePanGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGestureScreenEdgePan;
					} else if([recognizer isKindOfClass:[UILongPressGestureRecognizer class]]) {
						gestureFlags |= MDInvolvedGestureLongPress;
					}
					
				}
				_gestureFlags = gestureFlags;
			}
		}
	}];
	
	return (_currentTouches.count > 0);
}


-(NSDictionary *)context:(NSString *)contextName {
	NSDictionary *obj = [_contexts objectForKey:contextName];
	if(obj) {
		return obj;
	}
	
	return [self.parent context:contextName];
}

@end
