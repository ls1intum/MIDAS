//
//  MDTouch.m
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDTouch.h"

#import "MDCoding.h"

@interface MDTouch()<MDCoding>

@end

@implementation MDTouch

-(instancetype)initFromDictionary:(NSDictionary *)dict {
	self = [super init];
	if (self) {
		_timeOffset= [[dict valueForKey:@"timeOffset"] doubleValue];
		_location.x = [[dict valueForKey:@"locationX"] doubleValue];
		_location.y = [[dict valueForKey:@"locationY"] doubleValue];
	}
	return self;
}

-(instancetype)initWithLocation:(CGPoint)location timeOffset:(NSTimeInterval)timeOffset {
	self = [super init];
	if (self) {
		_location = location;
		_timeOffset = timeOffset;
	}
	return self;
}


-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t data_size = (sizeof(uint8_t)) + (sizeof(Float32) * 2) + sizeof(NSTimeInterval);
	uint8_t *data = malloc(data_size);
	Float32 f_data[4];
	
	data[0] = 0x40;
	
	f_data[0] = (Float32)_location.x;
	f_data[1] = (Float32)_location.y;
	
//	*((Float64*)(&f_data[2])) = (Float64)_timeOffset;
	memcpy(&f_data[2], &_timeOffset, sizeof(Float64));
	memcpy(&data[1], f_data, sizeof(Float32)*4);
	
	[encoder writeData:data ofLength:data_size];
}

-(CGPoint)point {
	return _location;
}

-(id<FBPoint>)fbPoint {
	return self;
}

@end
