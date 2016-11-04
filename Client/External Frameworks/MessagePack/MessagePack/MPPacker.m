//
//  MPPacker.m
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MPPacker.h"
#import "msgpack.h"

@interface MPPacker() {
	msgpack_sbuffer *_buffer;
	msgpack_packer *_pk;
}


@end

@implementation MPPacker

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

// Integer

-(BOOL)streamPackUInt8:(UInt8)i {
	return msgpack_pack_uint8(_pk, i) == 0;
}

-(BOOL)streamPackUInt16:(UInt16)i {
	return msgpack_pack_uint16(_pk, i) == 0;
}

-(BOOL)streamPackUInt32:(UInt32)i {
	return msgpack_pack_uint32(_pk, i) == 0;
}

-(BOOL)streamPackUInt64:(UInt64)i {
	return msgpack_pack_uint64(_pk, i) == 0;
}

-(BOOL)streamPackInt8:(SInt8)i {
	return msgpack_pack_int8(_pk, i) == 0;
}

-(BOOL)streamPackInt16:(SInt16)i {
	return msgpack_pack_int16(_pk, i) == 0;
}

-(BOOL)streamPackInt32:(SInt32)i {
	return msgpack_pack_int32(_pk, i) == 0;
}

-(BOOL)streamPackInt64:(SInt64)i {
	return msgpack_pack_int64(_pk, i) == 0;
}

// Float/Double

-(BOOL)streamPackFloat:(float)i {
	return msgpack_pack_float(_pk, i) == 0;
}

-(BOOL)streamPackDouble:(double)i {
	return msgpack_pack_double(_pk, i) == 0;
}

// Nil, True/False

-(BOOL)streamPackNil {
	return msgpack_pack_nil(_pk) == 0;
}

-(BOOL)streamPackTrue {
	return msgpack_pack_true(_pk) == 0;
}

-(BOOL)streamPackFalse {
	return msgpack_pack_false(_pk) == 0;
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

-(BOOL)packNSCodingObject:(id<NSCoding>)obj {
	NSData *data = [NSKeyedArchiver archivedDataWithRootObject:obj];
	[self packData:data];
	return YES;
}

-(NSData*)data {
	return [NSData dataWithBytes:_buffer->data length:_buffer->size];
}

@end
