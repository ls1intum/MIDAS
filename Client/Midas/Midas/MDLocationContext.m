//
//  MDLocationContext.m
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDLocationContext.h"

#import "MDLocationContextProvider.h"
#import "MDEncodeTypes.h"

@import CoreLocation;

@implementation MDLocationContext

- (instancetype)init {
	CLLocation *location = [[MDLocationContextProvider sharedProvider] lastLocation];
	if(location == nil) {
		return nil;
	}
	
	self = [super init];
	if (self) {
		_location = location;
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	// Content:
	// Type:u8 Lat:d Lon:d Altitude:d HAcc:d VAcc:d
	double content[7];
	size_t data_size = (sizeof(uint8_t)) + (sizeof(double) * 7);
	uint8_t *data = malloc(data_size);
	
	content[0] = _location.coordinate.latitude;
	content[1] = _location.coordinate.longitude;
	content[2] = _location.altitude;
	content[3] = _location.horizontalAccuracy;
	content[4] = _location.verticalAccuracy;
	content[5] = _location.speed;
	content[6] = _location.course;
	
	data[0] = kMDEncoded_Context_Location;
	
	memcpy(&data[1], &content[0], sizeof(double)*7);
	
	[encoder writeData:data ofLength:data_size];
}

@end
