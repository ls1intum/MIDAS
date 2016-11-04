//
//  MDSettings.m
//  Midas
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSettings.h"

@implementation MDSettings

-(instancetype)initWithDictionary:(NSDictionary *)dictionary {
	self = [super init];
	if (self) {
		_uploadHost = [dictionary valueForKey:@"uploadHost"];
		NSUInteger port = [[dictionary valueForKey:@"uploadPort"] unsignedIntegerValue];
		_uploadPort = (port == 0 ? 9990 : port);
		
		_downloadBaseURL = [NSURL URLWithString:[dictionary valueForKey:@"downloadBaseURL"]];
	}
	return self;
}


+(instancetype)settingsFromBundledPlist {
	static MDSettings *bundledSettings = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		NSURL *settingsURL = [[NSBundle mainBundle] URLForResource:@"midas" withExtension:@"plist"];
		NSDictionary *dictionary = [NSDictionary dictionaryWithContentsOfURL:settingsURL];
		bundledSettings = [[self alloc] initWithDictionary:dictionary];
	});
	return bundledSettings;
}

@end
