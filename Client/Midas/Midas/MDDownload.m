//
//  MDDownload.m
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownload.h"

#import "MDSettings.h"

@import UIKit;
@import MessagePack;

@interface MDDownload()

@property(readwrite,nonatomic,strong) NSURL *url;

@end

@implementation MDDownload

+(NSURLSession*)urlSession {
	static NSURLSession* session = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		NSURLSessionConfiguration *configuration = [NSURLSessionConfiguration defaultSessionConfiguration];
		configuration.requestCachePolicy = NSURLRequestReloadIgnoringLocalCacheData;
		session = [NSURLSession sessionWithConfiguration:configuration];
	});
	return session;
}

+(void)download:(NSURL*)url completionHandler:(void (^)(NSData *))completionHandler {
	NSLog(@"Downloading: %@",url);
	[[[MDDownload urlSession] dataTaskWithURL:url completionHandler:^(NSData * _Nullable data, NSURLResponse * _Nullable response, NSError * _Nullable error) {
		if([response isKindOfClass:[NSHTTPURLResponse class]]) {
			NSHTTPURLResponse *urlResponse = (NSHTTPURLResponse*)response;
			if(urlResponse.statusCode == 200) {
				completionHandler(data);
			}
		} else {
			completionHandler(nil);
		}
	}] resume];
}


@end
