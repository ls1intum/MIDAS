//
//  FBPoint.m
//  FBAnnotationClustering
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Infinum Ltd. All rights reserved.
//

#import "FBPoint.h"

@implementation FBPoint

-(instancetype)initWithX:(CGFloat)x andY:(CGFloat)y {
	return [self initWithPoint:CGPointMake(x, y)];
}

- (instancetype)initWithPoint:(CGPoint)p {
	self = [super init];
	if (self) {
		_point = p;
	}
	return self;
}

+(instancetype)point:(CGPoint)p {
	return [[self alloc] initWithPoint:p];
}

+(instancetype)pointWithX:(CGFloat)x andY:(CGFloat)y {
	return [[self alloc] initWithPoint:CGPointMake(x, y)];
}

@end
