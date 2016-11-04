//
//  MDFilterGroup.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilterGroup.h"

#import "MDFilter.h"
#import "MDFilterSimple.h"
#import "MDFilterValue.h"

@interface MDFilterGroup()<MDFilterUpdateDelegate>
@end

@implementation MDFilterGroup


-(instancetype)initRootFilterGroup {
	self = [super initWithName:@"Root" image:nil];
	if (self) {
		self.filters = [self createRootFilters];
	}
	return self;
}

+(NSBundle*)bundle {
	static NSBundle *bundle = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		bundle = [NSBundle bundleWithIdentifier:@"de.tum.in.www1.tg.midas"];
	});
	return bundle;
}

-(UIImage*)imageNamed:(NSString*)name {
	return [UIImage imageNamed:name inBundle:[MDFilterGroup bundle] compatibleWithTraitCollection:nil];
}

-(NSArray*)createRootFilters {
	NSMutableArray *filters = [[NSMutableArray alloc] init];
	
	[filters addObject:[self timeGroup]];
	[filters addObject:[self locationGroup]];
	[filters addObject:[self deviceGroup]];
	[filters addObject:[self activityGroup]];
	[filters addObject:[self weatherGroup]];
	
	return filters;
}

-(void)setFilters:(NSArray<MDFilter *> *)filters {
	_filters = filters;
	for (MDFilter *f in filters) {
		f.delegate = self;
	}
}


-(MDFilterGroup*)timeGroup {
	MDFilterGroup *time_grp = [[MDFilterGroup alloc] initWithName:@"Time" image:[self imageNamed:@"time/group"]];
	
	// Day of Week
	MDFilterValue *time_workingday = [MDFilterValue withValue:[NSNumber numberWithBool:YES] name:@"Working Day" image:[self imageNamed:@"time/weekday"]];
	MDFilterValue *time_weekday = [MDFilterValue withValue:[NSNumber numberWithBool:NO] name:@"Weekend" image:[self imageNamed:@"time/weekend"]];
	
	MDFilterSimple *dayOfWeek =
	[MDFilterSimple filterWithName:@"Day"
							 image:[self imageNamed:@"time/day"]
							forKey:@"weekday"
						 inContext:@"time_context"
				   availableValues:@[time_workingday,time_weekday]];
	
	// Time of Day
	MDFilterValue *time_morning = [MDFilterValue withValue:@"morning" name:@"Morning" image:[self imageNamed:@"time/morning"]];
	MDFilterValue *time_noon = [MDFilterValue withValue:@"noon" name:@"Noon" image:[self imageNamed:@"time/noon"]];
	MDFilterValue *time_afternoon = [MDFilterValue withValue:@"afternoon" name:@"Afternoon" image:[self imageNamed:@"time/afternoon"]];
	MDFilterValue *time_evening = [MDFilterValue withValue:@"evening" name:@"Evenining" image:[self imageNamed:@"time/evening"]];
	MDFilterValue *time_night = [MDFilterValue withValue:@"night" name:@"Night" image:[self imageNamed:@"time/night"]];
	
	MDFilterSimple *timeOfDay =
	[MDFilterSimple filterWithName:@"Time"
							 image:[self imageNamed:@"time/time"]
							forKey:@"time_of_day"
						 inContext:@"time_context"
				   availableValues:@[time_morning,time_noon,time_afternoon,time_evening,time_night]];
	
	
	time_grp.filters = @[dayOfWeek,timeOfDay];
	return time_grp;
}

-(MDFilter*)locationGroup {
	/*MDFilterGroup *location_grp = [[MDFilterGroup alloc] initWithName:@"Location" image:[self imageNamed:@"location/group"]];
	
	
	MDFilterValue *ctry_ger = [MDFilterValue withValue:@"germany" name:@"Germany" image:[self imageNamed:@"location/country"]];
	
	MDFilterSimple *country =
	[MDFilterSimple filterWithName:@"Country"
							 image:[self imageNamed:@"location/country"]
							forKey:@"country"
						 inContext:@"location_context"
				   availableValues:@[ctry_ger]];*/
	
	
	MDFilterValue *env_home = [MDFilterValue withValue:@"home" name:@"Home" image:[self imageNamed:@"location/home"]];
	MDFilterValue *env_office = [MDFilterValue withValue:@"office" name:@"Office" image:[self imageNamed:@"location/office"]];
	MDFilterValue *env_transit = [MDFilterValue withValue:@"transit" name:@"Transit" image:[self imageNamed:@"location/transit"]];
	MDFilterValue *env_outdoor = [MDFilterValue withValue:@"outdoor" name:@"Outdoor" image:[self imageNamed:@"location/outdoor"]];
	
	MDFilterSimple *environment =
	[MDFilterSimple filterWithName:@"Location"
							 image:[self imageNamed:@"location/group"]
							forKey:@"environment"
						 inContext:@"location_context"
				   availableValues:@[env_home,env_office,env_transit,env_outdoor]];
	
	
	//location_grp.filters = @[environment,country];
	return environment;
}

-(MDFilterGroup*)deviceGroup {
	MDFilterGroup *device_grp = [[MDFilterGroup alloc] initWithName:@"Device" image:[self imageNamed:@"device/group"]];
	
	MDFilterValue *b_unknown = [MDFilterValue withValue:@"unknown" name:@"Unknown" image:[self imageNamed:@"unknown"]];
	MDFilterValue *b_charging = [MDFilterValue withValue:@"charging" name:@"Charging" image:[self imageNamed:@"device/charging"]];
	MDFilterValue *b_charged = [MDFilterValue withValue:@"charged" name:@"Charged" image:[self imageNamed:@"device/charged"]];
	MDFilterValue *b_unplugged = [MDFilterValue withValue:@"unplugged" name:@"Unplugged" image:[self imageNamed:@"device/unplugged"]];
	
	MDFilterSimple *battery =
	[MDFilterSimple filterWithName:@"Battery"
							 image:[self imageNamed:@"device/battery"]
							forKey:@"battery.state"
						 inContext:@"device_state_context"
				   availableValues:@[b_charging,b_charged,b_unplugged,b_unknown]];
	
	
	device_grp.filters = @[battery];
	return device_grp;
}

-(MDFilter*)activityGroup {
	MDFilterValue *b_unknown = [MDFilterValue withValue:@"unknown" name:@"Unknown" image:[self imageNamed:@"unknown"]];
	MDFilterValue *b_resting = [MDFilterValue withValue:@"stationary" name:@"Resting" image:[self imageNamed:@"activity/resting"]];
	MDFilterValue *b_walking = [MDFilterValue withValue:@"walking" name:@"Walking" image:[self imageNamed:@"activity/group"]];
	MDFilterValue *b_running = [MDFilterValue withValue:@"running" name:@"Running" image:[self imageNamed:@"activity/running"]];
	MDFilterValue *b_cycling = [MDFilterValue withValue:@"cycling" name:@"Cycling" image:[self imageNamed:@"activity/cycling"]];
	MDFilterValue *b_driving = [MDFilterValue withValue:@"driving" name:@"Driving" image:[self imageNamed:@"activity/driving"]];
	
	MDFilterSimple *activity =
	[MDFilterSimple filterWithName:@"Activity"
							 image:[self imageNamed:@"activity/group"]
							forKey:@"concluded"
						 inContext:@"activity_context"
				   availableValues:@[b_resting,b_walking,b_running,b_cycling,b_driving,b_unknown]];
	
	return activity;
}

-(MDFilter*)weatherGroup {
	MDFilterValue *b_sunny = [MDFilterValue withValue:@"clear" name:@"Sunny" image:[self imageNamed:@"weather/sunny"]];
	MDFilterValue *b_rainy = [MDFilterValue withValue:@"rainy" name:@"Rainy" image:[self imageNamed:@"weather/rainy"]];
	MDFilterValue *b_cloudy = [MDFilterValue withValue:@"cloudy" name:@"Cloudy" image:[self imageNamed:@"weather/cloudy"]];
	
	MDFilterSimple *weather =
	[MDFilterSimple filterWithName:@"Weather"
							 image:[self imageNamed:@"weather/group"]
							forKey:@"condition"
						 inContext:@"weather_context"
				   availableValues:@[b_sunny,b_rainy,b_cloudy]];
	
	return weather;
}


-(BOOL)activeForFilterNamed:(NSString *)filterName {
	if(filterName == nil) {
		return NO;
	}
	for (MDFilter *f in self.filters) {
		if([f activeForFilterNamed:filterName] == YES) {
			return YES;
		}
	}
	return NO;
}


-(void)filterChanged:(MDFilter *)filter forFilterName:(NSString *)filterName {
	[self.delegate filterChanged:filter forFilterName:filterName];
}

-(BOOL)matchesRecording:(id)recording forFilterNamed:(NSString*)filterName {
	for (MDFilter *f in _filters) {
		if([f matchesRecording:recording forFilterNamed:filterName] == NO) {
			return NO;
		}
	}
	return YES;
}

-(void)resetForFilterNamed:(NSString *)filterName {
	for (MDFilter *f in _filters) {
		id del = f.delegate;
		f.delegate = nil;
		[f resetForFilterNamed:filterName];
		f.delegate = del;
	}
	[self.delegate filterChanged:self forFilterName:filterName];
}

@end
