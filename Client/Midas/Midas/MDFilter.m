//
//  MDFilter.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilter.h"

@implementation MDFilter

-(instancetype)initWithName:(NSString *)name image:(UIImage *)image {
	self = [super init];
	if (self) {
		_name = name;
		_image = image;
	}
	return self;
}

-(BOOL)activeForFilterNamed:(NSString *)filterName {
	return NO;
}

-(void)resetForFilterNamed:(NSString *)filterName {
	
}


@end
