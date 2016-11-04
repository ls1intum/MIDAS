//
//  MDBatteryContextProvider.h
//  Midas
//
//  Created by Thomas Günzel on 27/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContextProvider.h"

typedef enum : uint8_t {
	MDNotReachable = 0,
	MDReachableViaWiFi,
	MDReachableViaWWAN
} MDNetworkStatus;

@interface MDDeviceStateContextProvider : MDContextProvider

@property(readonly,nonatomic) MDNetworkStatus networkStatus;

@end
