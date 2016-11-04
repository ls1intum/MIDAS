//
//  MDDownloadedRecordings.m
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedRecordings.h"
#import "MDDownload.h"

#import "MDSettings.h"

@import MessagePack;

@interface MDDownloadedRecordings()<MPUnpackerDelegate>

@property(readwrite,nonatomic,strong) NSMutableArray *mutableRecordings;

@end

@implementation MDDownloadedRecordings


-(void)load {
	[MDDownload download:[self downloadURL] completionHandler:^(NSData *rawData) {
		[self handle:rawData];
	}];
}


-(void)postprocess {
	
}

-(NSURL *)downloadURL {
	return nil;
}

-(id)handleMsgpack:(NSDictionary *)dict {
	return nil;
}

-(void)handle:(NSData*)data {
	_mutableRecordings = [[NSMutableArray alloc] init];
	
	MPUnpacker *unpacker = [[MPUnpacker alloc] init];
	unpacker.delegate = self;
	[unpacker feed:data];
	
	_allRecordings = _mutableRecordings;
	
	[self postprocess];
	dispatch_async(dispatch_get_main_queue(), ^{
		[self.delegate recordings:self.allRecordings didDownload:self];
	});
	
}

-(void)unpacker:(MPUnpacker *)unpacker didUnpackObject:(id)object {
	if([object isKindOfClass:[NSDictionary class]] == NO) {
		NSLog(@"Couldn't unpack: %@",object);
		return;
	}
	NSDictionary *dict = object;
	
	id obj = [self handleMsgpack:dict];
	if(obj) {
		[_mutableRecordings addObject:obj];
	}
}

+(NSURL*)baseURL {
	static NSURL *baseURL = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		NSURL *baseBaseURL = [[MDSettings settingsFromBundledPlist] downloadBaseURL];
		NSString *appVersion = [[NSBundle mainBundle] objectForInfoDictionaryKey:(NSString *)kCFBundleVersionKey];
		baseURL = [NSURL URLWithString:[NSString stringWithFormat:@"%@/",appVersion] relativeToURL:baseBaseURL];
	});
	return baseURL;
}

+(NSURL*)deviceSpecificBaseURL {
	static NSURL *deviceBaseURL = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		NSURL *baseURL = [self baseURL];
		CGRect bounds = [[UIScreen mainScreen] bounds];
		NSString *dim = [NSString stringWithFormat:@"%.0fx%.0f",bounds.size.width,bounds.size.height];
		deviceBaseURL = [NSURL URLWithString:[NSString stringWithFormat:@"%@/",dim] relativeToURL:baseURL];
	});
	return deviceBaseURL;
}

@end
