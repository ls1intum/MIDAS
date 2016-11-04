//
//  MDMotionContextProvider.m
//  Midas
//
//  Created by Thomas Günzel on 17/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDMotionContextProvider.h"

@import CoreMotion;

@interface MDMotionContextProvider()

@property(readwrite,nonatomic,strong) CMMotionManager *manager;
@property(readwrite,nonatomic,strong) CMMotionActivityManager *activityManager;
@property(readwrite,nonatomic,strong) NSOperationQueue *motionQueue;

@property(readwrite,atomic,strong) CMMotionActivity *lastActivity;
@property(readwrite,atomic,strong) CMDeviceMotion *lastMotion;

@end

@implementation MDMotionContextProvider

- (instancetype)init {
	self = [super init];
	if (self) {
		_motionQueue = [[NSOperationQueue alloc] init];
		_motionQueue.qualityOfService = NSQualityOfServiceUtility;
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
	dispatch_async(dispatch_get_main_queue(), ^{
		[self startUpdatingMotion];
		//[self startUpdatingActivity];
	});
}

-(void)_stop {
	[self stopUpdating];
}

-(void)startUpdatingMotion {
	if(self.manager != nil) {
		return;
	}
	
	__weak typeof(self) weakSelf = self;
	
	self.manager = [[CMMotionManager alloc] init];
	self.manager.deviceMotionUpdateInterval = 0.1;
	NSLog(@"Starting Motion Updates");
	
	[_manager startDeviceMotionUpdatesToQueue:_motionQueue withHandler:^(CMDeviceMotion * _Nullable motion, NSError * _Nullable error) {
		weakSelf.lastMotion = motion;
	}];
}

-(void)startUpdatingActivity {
	if(self.activityManager != nil) {
		return;
	}
	
	if([CMMotionActivityManager isActivityAvailable] == NO) {
		NSLog(@"Activity Manager not available");
	}
	
	__weak typeof(self) weakSelf = self;
	
	self.activityManager = [[CMMotionActivityManager alloc] init];
	NSLog(@"Starting Activity Updates");
	
	[_activityManager startActivityUpdatesToQueue:_motionQueue withHandler:^(CMMotionActivity * _Nullable activity) {
		if(weakSelf.lastActivity == nil || [weakSelf.lastActivity.startDate compare:activity.startDate] == NSOrderedAscending) {
			if(activity.confidence >= weakSelf.lastActivity.confidence) {
				[[NSOperationQueue mainQueue] addOperationWithBlock:^{
					weakSelf.lastActivity = activity;
					//NSLog(@"Activity Update: %@",weakSelf.lastActivity);
				}];
			}
		}
	}];
}


-(void)stopUpdating {
	[_activityManager stopActivityUpdates];
	_activityManager = nil;
	
	[_manager stopDeviceMotionUpdates];
	_manager = nil;
}


@end
