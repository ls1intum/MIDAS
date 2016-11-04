//
//  MDDeviceStateContext.m
//  Midas
//
//  Created by Thomas Günzel on 27/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDeviceStateContext.h"

#import "MDEncodeTypes.h"

typedef NS_ENUM(uint8_t, MD_UIDeviceOrientation) {
	MD_UIDeviceOrientation_Unknown = 0,
	MD_UIDeviceOrientation_Portrait,
	MD_UIDeviceOrientation_PortraitUpsideDown, // does this even happen on the phone???
	MD_UIDeviceOrientation_LandscapeLeft,
	MD_UIDeviceOrientation_LandscapeRight,
	MD_UIDeviceOrientation_FaceUp,
	MD_UIDeviceOrientation_FaceDown,
};

MD_UIDeviceOrientation ui_to_mdDeviceOrientation(UIDeviceOrientation idiom) {
	switch (idiom) {
		case UIDeviceOrientationPortrait:           return MD_UIDeviceOrientation_Portrait;
		case UIDeviceOrientationPortraitUpsideDown: return MD_UIDeviceOrientation_PortraitUpsideDown;
		case UIDeviceOrientationLandscapeLeft:      return MD_UIDeviceOrientation_LandscapeLeft;
		case UIDeviceOrientationLandscapeRight:     return MD_UIDeviceOrientation_LandscapeRight;
		case UIDeviceOrientationFaceUp:             return MD_UIDeviceOrientation_FaceUp;
		case UIDeviceOrientationFaceDown:           return MD_UIDeviceOrientation_FaceDown;
		case UIDeviceOrientationUnknown:
		default:
			return MD_UIDeviceOrientation_Unknown;
	}
	return MD_UIDeviceOrientation_Unknown;
}

uint8_t ui_to_intBatteryState(UIDeviceBatteryState state) {
	switch (state) {
		case UIDeviceBatteryStateUnknown:
			return 0;
		case UIDeviceBatteryStateUnplugged:
			return 1;
		case UIDeviceBatteryStateCharging:
			return 2;
		case UIDeviceBatteryStateFull:
			return 3;
	}
}

@implementation MDDeviceStateContext

-(instancetype)init {
	self = [super init];
	if (self) {
		UIDevice *device = [UIDevice currentDevice];
		_batteryState = device.batteryState;
		_batteryLevel = device.batteryLevel;
		_orientation = device.orientation;
		
		_networkStatus = [[MDDeviceStateContextProvider sharedProvider] networkStatus];
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t data_size = (5 * sizeof(uint8_t));
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Context_DeviceState;
	data[1] = ui_to_mdDeviceOrientation(_orientation);
	data[2] = ui_to_intBatteryState(_batteryState);
	data[3] = (uint8_t)round(_batteryLevel*100.0);
	data[4] = _networkStatus;
		
	[encoder writeData:data ofLength:data_size];
}

@end
