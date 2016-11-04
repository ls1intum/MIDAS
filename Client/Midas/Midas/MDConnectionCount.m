//
//  MDSegueConnection.m
//  Midas
//
//  Created by Thomas Günzel on 17/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDConnectionCount.h"

@implementation MDConnectionCount

- (instancetype)init {
	self = [super init];
	if (self) {
		_count = 0;
		_all = 0;
	}
	return self;
}

@end
