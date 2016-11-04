//
//  MDLocationContextProvider.m
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDLocationContextProvider.h"

@import CoreLocation;

BOOL const kMidasAskForLocationServices = NO;

@interface MDLocationContextProvider()<CLLocationManagerDelegate>

@property(readwrite,nonatomic,strong) CLLocation *lastLocation;
@property(readwrite,nonatomic,strong) CLLocationManager *manager;

@property(readwrite,nonatomic) BOOL available;
@property(readwrite,nonatomic) BOOL updating;

@end

@implementation MDLocationContextProvider

- (instancetype)init {
	self = [super init];
	if (self) {
	}
	return self;
}

-(void)_start {
	dispatch_async(dispatch_get_main_queue(), ^{
		[self startUpdating];
	});
}

-(void)_stop {
	[self stopUpdating];
}

-(void)startUpdating {
	if(self.updating == YES) {
		return;
	}
	
	self.lastLocation = nil;
	
	if([CLLocationManager locationServicesEnabled] == NO) {
		self.available = NO;
		NSLog(@"Location Services disabled");
		return;
	}
	
	if([CLLocationManager significantLocationChangeMonitoringAvailable] == NO) {
		self.available = NO;
		NSLog(@"Significant Location Change Monitoring not available");
		return;
	}
	
	BOOL ask = NO;
	switch([CLLocationManager authorizationStatus]) {
		case kCLAuthorizationStatusNotDetermined:
			if(kMidasAskForLocationServices) {
				ask = YES;
				break;
			}
		case kCLAuthorizationStatusDenied:
		case kCLAuthorizationStatusRestricted:
			NSLog(@"Authorization Status: %d",[CLLocationManager authorizationStatus]);
			return;
		default:
			break;
	}
	
	if(self.manager == nil) {
		self.manager = [[CLLocationManager alloc] init];
	}
	
	self.manager.pausesLocationUpdatesAutomatically = YES;
	self.manager.delegate = self;
	self.manager.desiredAccuracy = kCLLocationAccuracyHundredMeters;
	self.manager.distanceFilter = 100.0;
	
	if(ask == YES) {
		[self.manager requestWhenInUseAuthorization];
	}
	
	NSLog(@"Starting Updates");
	self.updating = YES;
	[self.manager startUpdatingLocation];
}

-(void)stopUpdating {
	NSLog(@"Stopping Updates");
	[_manager stopUpdatingLocation];
	self.updating = NO;
}

-(void)locationManager:(CLLocationManager *)manager didChangeAuthorizationStatus:(CLAuthorizationStatus)status {
	NSLog(@"Authorization Status: %d",status);
}

-(void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray<CLLocation *> *)locations {
	[locations enumerateObjectsUsingBlock:^(CLLocation *obj, NSUInteger idx, BOOL *stop) {
		if(self.lastLocation == nil || [self.lastLocation.timestamp compare:obj.timestamp] == NSOrderedAscending) {
			self.lastLocation = obj;
			//NSLog(@"Location Update: %@",self.lastLocation);
		}
	}];
}

-(void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error {
	if(error.code == 0) {
		return;
	}
	self.updating = NO;
	NSLog(@"Error: %@",error);
}

@end
