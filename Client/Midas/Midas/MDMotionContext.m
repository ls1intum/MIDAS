//
//  MDMotionContext.m
//  Midas
//
//  Created by Thomas Günzel on 17/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDMotionContext.h"
#import "MDMotionContextProvider.h"

#import "MDEncodeTypes.h"

@import CoreMotion;

@implementation MDMotionContext

- (instancetype)init {
	CMDeviceMotion *motion = [[MDMotionContextProvider sharedProvider] lastMotion];
	if(motion == nil) {
		return nil;
	}
	
	self = [super init];
	if (self) {
		_motion = motion;
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	// Content:
	// Type:u8 Lat:d Lon:d Altitude:d HAcc:d VAcc:d
	double content[12];
	size_t data_size = (sizeof(uint8_t)) + (sizeof(double) * 12);
	uint8_t *data = malloc(data_size);
	
	
	CMAttitude *attitude = _motion.attitude;
	content[0] = attitude.roll;
	content[1] = attitude.pitch;
	content[2] = attitude.yaw;
	
	CMRotationRate rate = _motion.rotationRate;
	content[3] = rate.x;
	content[4] = rate.y;
	content[5] = rate.z;
	
	CMAcceleration grav = _motion.gravity;
	content[6] = grav.x;
	content[7] = grav.y;
	content[8] = grav.z;
	
	CMAcceleration user = _motion.userAcceleration;
	content[9] = user.x;
	content[10] = user.y;
	content[11] = user.z;
	
	
	data[0] = kMDEncoded_Context_Motion;
	
	memcpy(&data[1], &content[0], sizeof(double)*12);
	
	[encoder writeData:data ofLength:data_size];
}


@end
