//
//  MDRecordedGestures.m
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDRecordedGestures.h"

#import "MDGesture.h"
#import "MDEncoder.h"

#import "MDEncodeTypes.h"

#import "MDDeviceStateContext.h"
#import "MDTimeContext.h"
#import "MDLocationContext.h"
#import "MDActivityContext.h"
#import "MDMotionContext.h"

@interface MDRecordedGestures()<MDCoding>

@property(readwrite,nonatomic) NSTimeInterval initialTimestamp;
@property(readwrite,nonatomic,strong) MDGesture *currentGesture;

//@property(readwrite,nonatomic,strong) MDDeviceContext *deviceContext;
//@property(readwrite,nonatomic,strong) MDLocationContext *locationContext;

@end

@implementation MDRecordedGestures

- (instancetype)initFromDictionary:(NSDictionary *)dict {
	if([[dict valueForKey:@"type"] isEqualToString:@"touch_recording"] == NO) {
		return nil;
	}
	self = [super init];
	if (self) {
		_viewControllerName = [dict valueForKey:@"viewIdentifier"];
		NSMutableArray *gestures = [NSMutableArray array];
		for (NSDictionary *d in [dict valueForKey:@"gestures"]) {
			MDGesture *gesture = [[MDGesture alloc] initFromDictionary:d];
			if(gesture) {
				gesture.parent = self;
				[gestures addObject:gesture];
			}
		}
		_gestures = gestures;
		
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

-(instancetype)initWithViewControllerName:(NSString *)viewControllerName encoder:(MDEncoder *)encoder {
	self = [super init];
	if (self) {
		_viewControllerName = viewControllerName;
		_encoder = encoder;
		_initialTimestamp = [[NSProcessInfo processInfo] systemUptime]; // to get the time-delta later
		
		[_encoder encodeObject:self];

	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t name_size = MIN(_viewControllerName.length,254);
	size_t data_size = (sizeof(uint8_t) << 1) + name_size;
	uint8_t *data = malloc(data_size);
	data[0] = kMDEncoded_RecordedGesture;
	data[1] = name_size;
	
	memcpy(&data[2], _viewControllerName.UTF8String, name_size);
	
	[_encoder writeData:data ofLength:data_size];
	[_encoder encodeObject:[MDDeviceStateContext currentContext]];
	[_encoder encodeObject:[MDTimeContext currentContext]];
	[_encoder encodeObject:[MDLocationContext currentContext]];
	[_encoder encodeObject:[MDActivityContext currentContext]];
	[_encoder encodeObject:[MDMotionContext currentContext]];
}

-(void)logEvent:(UIEvent *)event {
	if(_currentGesture == nil) {
		NSTimeInterval currentTimestamp = [[NSProcessInfo processInfo] systemUptime];
		_currentGesture = [[MDGesture alloc] initWithTimeOffset:(currentTimestamp - _initialTimestamp)];
	}
	
	if([_currentGesture handleTouches:event.allTouches] == NO) {
		[_encoder encodeObject:_currentGesture];
		_currentGesture = nil;
	}
}


-(NSDictionary *)context:(NSString *)contextName {
	NSDictionary *obj = [_contexts objectForKey:contextName];
	if(obj) {
		return obj;
	}
	
	return nil;
}

@end
