//
//  MDActivityContext.m
//  Midas
//
//  Created by Thomas Günzel on 17/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDActivityContext.h"
#import "MDMotionContextProvider.h"
#import "MDEncodeTypes.h"

@import CoreMotion;

@implementation MDActivityContext

- (instancetype)init {
	CMMotionActivity *activity = [[MDMotionContextProvider sharedProvider] lastActivity];
	if(activity == nil) {
		return nil;
	}
	
	self = [super init];
	if (self) {
		_activity = activity;
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	// Content:
	// Type:u8 Lat:d Lon:d Altitude:d HAcc:d VAcc:d
	size_t data_size = (sizeof(uint8_t) << 1);
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Context_Activity;
	
	uint8_t activity;
	uint8_t confidence = (uint8_t)_activity.confidence;
	
	activity = (confidence << 6)
	| (_activity.unknown << 5)
	| (_activity.stationary << 4)
	| (_activity.walking << 3)
	| (_activity.running << 2)
	| (_activity.automotive << 1)
	| (_activity.cycling);
	
	data[1] = activity;
	
	[encoder writeData:data ofLength:data_size];
}


@end
