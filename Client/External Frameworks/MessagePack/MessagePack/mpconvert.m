//
//  mpconvert.m
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "mpconvert.h"


#import "MPCoding.h"

id mp_convert(msgpack_object *object) {
	return mp_convert_mapped(object, nil);
}

id mp_convert_mapped(msgpack_object *object, NSDictionary *class_mapping) {
	switch (object->type) {
		case MSGPACK_OBJECT_NIL:
			return [NSNull null];
		case MSGPACK_OBJECT_NEGATIVE_INTEGER:
			return [NSNumber numberWithLongLong:object->via.i64];
		case MSGPACK_OBJECT_POSITIVE_INTEGER:
			return [NSNumber numberWithUnsignedLongLong:object->via.u64];
		case MSGPACK_OBJECT_BOOLEAN:
			return [NSNumber numberWithBool:object->via.boolean];
		case MSGPACK_OBJECT_FLOAT:
			return [NSNumber numberWithDouble:object->via.f64];
		case MSGPACK_OBJECT_BIN:
			return [NSData dataWithBytes:object->via.bin.ptr length:object->via.bin.size];
		case MSGPACK_OBJECT_STR:
			return [[NSString alloc] initWithBytes:object->via.str.ptr length:object->via.str.size encoding:NSUTF8StringEncoding];
		case MSGPACK_OBJECT_ARRAY: {
			msgpack_object_array array = object->via.array;
			NSMutableArray *ns = [[NSMutableArray alloc] initWithCapacity:array.size];
			for (uint32_t i = 0; i < array.size; i++) {
				msgpack_object o = array.ptr[i];
				[ns addObject:mp_convert(&o)];
			}
			return ns;
		}
		case MSGPACK_OBJECT_MAP: {
			msgpack_object_map *map = &object->via.map;
			NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:map->size];
			for (uint32_t i = 0; i < map->size; i++) {
				msgpack_object_kv *kv = &map->ptr[i];
				id key = mp_convert(&kv->key);
				id value = mp_convert(&kv->val);
				if([key conformsToProtocol:@protocol(NSCopying)] == NO) {
					NSLog(@"Couldn't add object %@ as it doesn't conform to NSCopying.",key);
					continue;
				}
				[dict setObject:value forKey:key];
			}
			if(class_mapping != nil) {
				NSString *type = [dict objectForKey:@"type"];
				if(type != nil) {
					NSString *className = [class_mapping objectForKey:type];
					if(className != nil) {
						Class c = NSClassFromString(className);
						if(c != nil) {
							return [[c alloc] initWithMsgpackDictionary:dict];
						}
					}
				}
			}
			return dict;
		}
		default:
			NSLog(@"Object type %u not supported",object->type);
			break;
	}
	return nil;
}

