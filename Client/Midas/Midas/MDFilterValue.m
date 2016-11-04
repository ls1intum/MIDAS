//
//  MDFilterValue.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilterValue.h"

@implementation MDFilterValue

-(instancetype)initWithName:(NSString *)name image:(id)image acceptedValue:(id)acceptedValue {
	self = [super initWithName:name image:image];
	if (self) {
		_acceptedValue = acceptedValue;
	}
	return self;
}

+(instancetype)withValue:(id)acceptedValue name:(NSString *)name image:(UIImage *)image {
	return [[self alloc] initWithName:name image:image acceptedValue:acceptedValue];
}

@end
