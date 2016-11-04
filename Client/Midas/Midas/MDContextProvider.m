//
//  MDContextProvider.m
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContextProvider.h"

@interface MDContextProvider()

@property(readwrite,nonatomic) NSUInteger needed;

@end

@implementation MDContextProvider

+(instancetype)sharedProvider {
	static __strong id provider = nil;
	static dispatch_once_t onceToken;
	
	dispatch_once(&onceToken, ^{
		provider = [[self alloc] init];
	});
	
	return provider;
}


- (instancetype)init {
	self = [super init];
	if (self) {
		_needed = 0;
	}
	return self;
}

-(void)dealloc {
	NSLog(@"I'm dead %@",self);
}

-(void)increaseNeeded {
	self.needed = self.needed+1;
}

-(void)decreaseNeeded {
	if(self.needed == 0) {
		return;
	}
	self.needed = self.needed-1;
}

-(void)setNeeded:(NSUInteger)needed {
	if(_needed == needed) {
		return;
	}
	
	if(_needed > 0 && needed == 0) { // decremented to 0
		[self _stop];
	} else if(_needed == 0 && needed > 0) {
		[self _start];
	}
	_needed = needed;
}

-(void)_start {
	
}

-(void)_stop {
	
}

@end
