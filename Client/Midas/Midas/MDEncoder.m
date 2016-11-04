//
//  MDEncoder.m
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDEncoder.h"

#import "MDCoding.h"

@interface MDEncoder() {
	int fd;
}

@property(readwrite,nonatomic,strong) NSString *path;
@property(readwrite,nonatomic,strong) NSMutableArray *queuedActions;

@end

@implementation MDEncoder

- (instancetype)initWithPath:(NSString *)path {
	self = [super init];
	if (self) {
		_path = path;
		_queuedActions = [[NSMutableArray alloc] init];
		fd = -1;
		[self open];
	}
	return self;
}

-(void)dealloc {
	if(fd < 0) {return;}
	close(fd);
	fd = -1;
}


-(void)open {
	fd = open(_path.UTF8String, O_WRONLY | O_TRUNC | O_CREAT, (S_IRUSR|S_IRGRP|S_IROTH|S_IWUSR|S_IWGRP|S_IWOTH));
	if(fd < 0) {
		NSLog(@"Couldn't open file.");
		return;
	}
}

-(void)encodeObject:(id<MDCoding>)object {
	if(object == nil) {
		return;
	}
	[object encodeWithEncoder:self];
}

-(void)writeData:(uint8_t *)data ofLength:(size_t)length {
	if(fd < 0) { return; }
	write(fd, (const void *)data, length);
}

-(void)queueAction:(id<MDCoding>)action {
	[_queuedActions addObject:action];
}

-(void)flushActions {
	for (id<MDCoding> action in _queuedActions) {
		[action encodeWithEncoder:self];
	}
}

@end
