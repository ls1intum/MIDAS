//
//  MDRecordedAction.m
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDRecordedAction.h"

#import "MDEncodeTypes.h"

#import "MDDeviceStateContext.h"
#import "MDTimeContext.h"
#import "MDLocationContext.h"
#import "MDActivityContext.h"
#import "MDMotionContext.h"

@implementation MDRecordedAction

- (instancetype)initFromDictionary:(NSDictionary *)dict {
	if([[dict valueForKey:@"type"] isEqualToString:@"action"] == NO) {
		return nil;
	}
	self = [super init];
	if (self) {
		_viewController = [dict valueForKey:@"viewIdentifier"];
		_selector = [dict valueForKey:@"selector"];
		_senderKeyPath = [dict valueForKey:@"key"];
		
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

-(instancetype)initWithViewController:(NSString *)viewController selector:(NSString *)selector senderKey:(NSString *)senderKey {
	self = [super init];
	if (self) {
		_viewController = viewController;
		_selector = selector;
		_senderKeyPath = senderKey;
	}
	return self;
}

-(void)encodeWithEncoder:(MDEncoder *)encoder {
	size_t vc_size = MIN(_viewController.length,254);
	size_t selector_size = MIN(_selector.length,254);
	size_t sender_key_size = MIN(_senderKeyPath.length,254);
	
	size_t data_size = (4 * sizeof(uint8_t)) + vc_size + selector_size + sender_key_size;
	uint8_t *data = malloc(data_size);
	
	data[0] = kMDEncoded_Action;
	
	size_t pos = 1;
	
	// VC
	data[pos] = vc_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _viewController.UTF8String, vc_size);
	pos += vc_size;
	
	// Sel
	data[pos] = selector_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _selector.UTF8String, selector_size);
	pos += selector_size;
	
	// Key
	data[pos] = sender_key_size;
	pos += sizeof(uint8_t);
	memcpy(&data[pos], _senderKeyPath.UTF8String, sender_key_size);
	pos += sender_key_size;
	
	[encoder writeData:data ofLength:data_size];
	[encoder encodeObject:[[MDDeviceStateContext alloc] init]];
	[encoder encodeObject:[[MDTimeContext alloc] init]];
}



-(NSDictionary *)context:(NSString *)contextName {
	NSDictionary *obj = [_contexts objectForKey:contextName];
	if(obj) {
		return obj;
	}
	
	return nil;
}


@end
