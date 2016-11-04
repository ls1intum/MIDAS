//
//  MPUnpacker.m
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MPUnpacker.h"
#import "msgpack.h"

#import "MPCoding.h"

#import "mpconvert.h"

@interface MPUnpacker() {
	msgpack_unpacker *_unp;
}

@property(readwrite,nonatomic,strong) NSMutableDictionary *classMappings;

@end

@implementation MPUnpacker


- (instancetype)init
{
	self = [super init];
	if (self) {
		_unp = msgpack_unpacker_new(MSGPACK_UNPACKER_INIT_BUFFER_SIZE);
		_classMappings = [[NSMutableDictionary alloc] init];
	}
	return self;
}

-(void)dealloc {
	msgpack_unpacker_free(_unp);
}


-(void)feed:(NSData *)data {
	[self feed:data.bytes length:data.length];
}

-(void)feed:(const char *)data length:(size_t)len {
	if(msgpack_unpacker_buffer_capacity(_unp) < len) {
		msgpack_unpacker_reserve_buffer(_unp, len);
		if(msgpack_unpacker_buffer_capacity(_unp) < len) {
			NSLog(@"Couldn't allocate buffer!");
			return;
		}
	}
	
	memcpy(msgpack_unpacker_buffer(_unp), data, len);
	
	msgpack_unpacker_buffer_consumed(_unp, len);
	
	[self tryUnpacking];
}

-(void)setClass:(Class)c forType:(NSString *)type {
	NSString *classStr = NSStringFromClass(c);
	if ([c conformsToProtocol:@protocol(MPCoding)] == NO) {
		NSLog(@"Mapping not added: Class %@ is not conforming to protocol MPCoding.",classStr);
		return;
	}
	[_classMappings setObject:classStr forKey:type];
}


-(void)tryUnpacking {
	msgpack_unpacked result;
	msgpack_unpacked_init(&result);
	
	while(msgpack_unpacker_next(_unp, &result)) {
		id unpacked = mp_convert_mapped(&result.data, _classMappings);
		self.lastUnpackedObject = unpacked;
		[self.delegate unpacker:self didUnpackObject:unpacked];
	}
	
	msgpack_unpacked_destroy(&result);
}



@end
