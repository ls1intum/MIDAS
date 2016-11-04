//
//  MDDeviceStateContext.h
//  Midas
//
//  Created by Thomas Günzel on 27/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContext.h"

#import "MDDeviceStateContextProvider.h"

@import UIKit;

@interface MDDeviceStateContext : MDContext

@property(readonly,nonatomic) UIDeviceOrientation orientation;
@property(readonly,nonatomic) float batteryLevel;
@property(readonly,nonatomic) UIDeviceBatteryState batteryState;

@property(readonly,nonatomic) MDNetworkStatus networkStatus;


@end
