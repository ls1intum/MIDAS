//
//  MDRecordedSegue.m
//  Midas
//
//  Created by Thomas Günzel on 18/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDRecordedSegue.h"

#import "MDEncodeTypes.h"

#import "MDDeviceStateContext.h"
#import "MDTimeContext.h"
#import "MDLocationContext.h"
#import "MDActivityContext.h"
#import "MDMotionContext.h"


@implementation MDRecordedSegue

- (instancetype)initFromDictionary:(NSDictionary *)dict {
	if([[dict valueForKey:@"type"] isEqualToString:@"segue"] == NO) {
		return nil;
	}
	self = [super init];
	if (self) {
		_sourceViewController = [dict valueForKey:@"sourceViewIdentifier"];
		_destinationViewController = [dict valueForKey:@"destinationViewIdentifier"];
		_segueName = [dict valueForKey:@"name"];
		_senderKey = [dict valueForKey:@"senderKey"];
		
		NSMutableDictionary *ctxs = [NSMutableDictionary dictionary];
		for (NSString *k in dict.keyEnumerator) {
			if([k hasSuffix:@"_context"]) {
				[ctxs setValue:[dict valueForKey:k] forKey:k];
			}
		}
		_contexts = ctxs;
	}
	return self;
}

-(instancetype)initWithSegueName:(NSString *)segueName sourceViewController:(NSString *)source destinationViewController:(NSString *)destinationViewController senderKey:(NSString *)senderKey {
	self = [super init];
	if (self) {
		_segueName = segueName;
		_sourceViewController = source;
		_destinationViewController = destinationViewController;
		_senderKey = senderKey;
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t seg_size = MIN(_segueName.length,254);
	size_t src_size = MIN(_sourceViewController.length,254);
	size_t dst_size = MIN(_destinationViewController.length,254);
	size_t key_size = MIN(_senderKey.length,254);
	
	size_t data_size = (5 * sizeof(uint8_t)) + seg_size + src_size + dst_size + key_size;
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Segue;
	
	size_t pos = 1;
	
	// Seg
	data[pos] = seg_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _segueName.UTF8String, seg_size);
	pos += seg_size;
	
	// Sel
	data[pos] = src_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _sourceViewController.UTF8String, src_size);
	pos += src_size;
	
	// Key
	data[pos] = dst_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _destinationViewController.UTF8String, dst_size);
	pos += dst_size;
	
	// Key
	data[pos] = key_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _senderKey.UTF8String, key_size);
	pos += key_size;
	
	[encoder writeData:data ofLength:data_size];
	[encoder encodeObject:[MDDeviceStateContext currentContext]];
	[encoder encodeObject:[MDTimeContext currentContext]];
	[encoder encodeObject:[MDLocationContext currentContext]];
	[encoder encodeObject:[MDActivityContext currentContext]];
	[encoder encodeObject:[MDMotionContext currentContext]];
}


-(NSDictionary *)context:(NSString *)contextName {
	NSDictionary *obj = [_contexts objectForKey:contextName];
	if(obj) {
		return obj;
	}
	
	return nil;
}


@end
