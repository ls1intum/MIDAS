//
//  MPKeyedArchiver.m
//  MessagePack
//
//  Created by Thomas Günzel on 21/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MPKeyedArchiver.h"

#import "MPPacker.h"

#import "MPCoding.h"

#import "msgpack.h"

@interface MPKeyedArchiver() {
	msgpack_sbuffer *_buffer;
	msgpack_packer *_pk;
}

@end


@implementation MPKeyedArchiver

- (instancetype)init {
	self = [super init];
	if (self) {
		_buffer = msgpack_sbuffer_new();
		_pk = msgpack_packer_new(_buffer, msgpack_sbuffer_write);
	}
	return self;
}

-(void)dealloc {
	msgpack_packer_free(_pk);
	msgpack_sbuffer_free(_buffer);
}


// Container (Map, Array)

-(BOOL)streamPackArray:(size_t)arraySize {
	return msgpack_pack_array(_pk, arraySize) == 0;
}

-(BOOL)streamPackMap:(size_t)mapSize {
	return msgpack_pack_map(_pk, mapSize) == 0;
}


// Bin/String

-(BOOL)streamPackBin:(size_t)binSize {
	return msgpack_pack_bin(_pk, binSize) == 0;
}

-(BOOL)streamPackBinBody:(const void*)body length:(size_t)l {
	return msgpack_pack_bin_body(_pk, body, l) == 0;
}

-(BOOL)streamPackStr:(size_t)strLength {
	return msgpack_pack_str(_pk, strLength) == 0;
}

-(BOOL)streamPackStrBody:(const void*)body length:(size_t)l {
	return msgpack_pack_str_body(_pk, body, l) == 0;
}


#pragma mark - Objective-C/Cocoa

-(BOOL)pack:(id)obj {
	if([obj isKindOfClass:[NSArray class]]) {
		return [self packArray:obj];
	} else if([obj isKindOfClass:[NSDictionary class]]) {
		return [self packDictionary:obj];
	} else if([obj isKindOfClass:[NSNumber class]]) {
		return [self packNumber:obj];
	} else if([obj isKindOfClass:[NSString class]]) {
		return [self packString:obj];
	} else if([obj isKindOfClass:[NSData class]]) {
		return [self packData:obj];
	} else if([obj conformsToProtocol:@protocol(NSCoding)]) {
		return [self packNSCodingObject:obj];
	} else if([obj respondsToSelector:@selector(description)]) {
		return [self packString:[obj description]];
	}
	return NO;
}

-(BOOL)packArray:(NSArray *)array {
	[self streamPackArray:array.count];
	
	for (id obj in array) {
		[self pack:obj];
	}
	
	return YES;
}

-(BOOL)packDictionary:(NSDictionary *)dictionary {
	[self streamPackMap:dictionary.count];
	
	[dictionary enumerateKeysAndObjectsUsingBlock:^(id  _Nonnull key, id  _Nonnull obj, BOOL * _Nonnull stop) {
		[self pack:key];
		[self pack:obj];
	}];
	
	return YES;
}

-(BOOL)packNumber:(NSNumber *)number {
	//	const char *objCType = number.objCType;
	//	const char type = objCType[0];
	//	switch (type) {
	//		case 'c':
	//			return msgpack_pack_char(_pk, number.charValue) == 0;
	//		case 'C':
	//			return msgpack_pack_unsigned_char(_pk, number.unsignedCharValue) == 0;
	//		case 'i':
	//			return msgpack_pack_int(_pk, number.intValue) == 0;
	//		case 'I':
	//			return msgpack_pack_unsigned_int(_pk, number.unsignedIntValue) == 0;
	//		case 's':
	//			return msgpack_pack_short(_pk, number.shortValue) == 0;
	//		case 'S':
	//			return msgpack_pack_unsigned_short(_pk, number.unsignedShortValue) == 0;
	//		case 'l':
	//			return msgpack_pack_long(_pk, number.longValue) == 0;
	//		case 'L':
	//			return msgpack_pack_unsigned_long(_pk, number.unsignedLongValue) == 0;
	//		case 'q':
	//			return msgpack_pack_long_long(_pk, number.longLongValue) == 0;
	//		case 'Q':
	//			return msgpack_pack_unsigned_long_long(_pk, number.unsignedLongLongValue) == 0;
	//		case 'f':
	//			return msgpack_pack_float(_pk, number.floatValue) == 0;
	//		case 'd':
	//			return msgpack_pack_double(_pk, number.doubleValue) == 0;
	//		case 'B':
	//			return (number.boolValue == YES ? msgpack_pack_true(_pk) : msgpack_pack_false(_pk)) == 0;
	//		case 'v':
	//			return msgpack_pack_nil(_pk) == 0;
	//		default:
	//			break;
	//	}
	
	void *value;
	[number getValue:&value];
	return [self packObjCType:number.objCType addr:value];
}

-(BOOL)packObjCType:(const char *)type addr:(const void *)addr {
	switch (type[0]) {
		case 'c':
			return msgpack_pack_char(_pk, *(char*)addr) == 0;
		case 'C':
			return msgpack_pack_unsigned_char(_pk, *(unsigned char*)addr) == 0;
		case 'i':
			return msgpack_pack_int(_pk, *(int*)addr) == 0;
		case 'I':
			return msgpack_pack_unsigned_int(_pk, *(unsigned int*)addr) == 0;
		case 's':
			return msgpack_pack_short(_pk, *(short*)addr) == 0;
		case 'S':
			return msgpack_pack_unsigned_short(_pk, *(unsigned short*)addr) == 0;
		case 'l':
			return msgpack_pack_long(_pk, *(long*)addr) == 0;
		case 'L':
			return msgpack_pack_unsigned_long(_pk, *(unsigned long*)addr) == 0;
		case 'q':
			return msgpack_pack_long_long(_pk, *(long long*)addr) == 0;
		case 'Q':
			return msgpack_pack_unsigned_long_long(_pk, *(unsigned long long*)addr) == 0;
		case 'f':
			return msgpack_pack_float(_pk, *(float*)addr) == 0;
		case 'd':
			return msgpack_pack_double(_pk, *(double*)addr) == 0;
		case 'B':
			return (*(BOOL*)addr == YES ? msgpack_pack_true(_pk) : msgpack_pack_false(_pk)) == 0;
		case 'v':
			return msgpack_pack_nil(_pk) == 0;
		case '@':
			return [self pack:*(const id*)addr];
		default:
			break;
	}
	return NO;
}



-(BOOL)packString:(NSString *)string {
	size_t len = string.length;
	[self streamPackStr:len];
	[self streamPackStrBody:string.UTF8String length:len];
	return YES;
}

-(BOOL)packData:(NSData *)data {
	size_t len = data.length;
	[self streamPackBin:len];
	[self streamPackBinBody:data.bytes length:len];
	return YES;
}

-(BOOL)packNSCodingObject:(id<MPCoding>)obj {
	NSUInteger count = [obj encodeKeyCount]+1;
	[self streamPackMap:count];
	
	// auto-add type
	[self packString:@"type"];
	NSString *type;
	if([obj respondsToSelector:@selector(messagePackType)] == YES) {
		type = [obj messagePackType];
	} else {
		type = NSStringFromClass([((id)obj) class]);
	}
	[self packString:type];
	
	// encode the rest
	[obj encodeWithCoder:self];
	
	return YES;
}

-(NSData*)data {
	return [NSData dataWithBytes:_buffer->data length:_buffer->size];
}

#pragma mark - NSCoder


-(void)encodeValueOfObjCType:(const char *)type at:(const void *)addr {
	if([self packObjCType:type addr:addr] == NO) {
		NSLog(@"Couldn't pack value %p of type %s",addr,type);
		return;
	}
}

-(void)encodeObject:(id)objv forKey:(NSString *)key {
	[self packString:key];
	[self pack:objv];
}

@end
