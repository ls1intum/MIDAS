//
//  MDBatteryContextProvider.m
//  Midas
//
//  Created by Thomas Günzel on 27/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDeviceStateContextProvider.h"
#import "Reachability.h"

@import UIKit;

@interface MDDeviceStateContextProvider()

@property(readwrite,nonatomic,strong) Reachability *reachability;

@end

@implementation MDDeviceStateContextProvider

- (instancetype)init {
	self = [super init];
	if (self) {
		_reachability = [Reachability reachabilityForInternetConnection];
	}
	return self;
}

+(instancetype)sharedProvider {
	static __strong id provider = nil;
	static dispatch_once_t onceToken;
	
	dispatch_once(&onceToken, ^{
		provider = [[self alloc] init];
	});
	
	return provider;
}

-(void)_start {
	[UIDevice currentDevice].batteryMonitoringEnabled = YES;
	[_reachability startNotifier];
}

-(void)_stop {
	[UIDevice currentDevice].batteryMonitoringEnabled = NO;
	[_reachability stopNotifier];
}

-(MDNetworkStatus)networkStatus {
	return (MDNetworkStatus)[_reachability currentReachabilityStatus];
}

@end
