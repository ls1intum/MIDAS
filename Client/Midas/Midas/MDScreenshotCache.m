//
//  MDScreenshotCache.m
//  Midas
//
//  Created by Thomas Günzel on 18/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDScreenshotCache.h"
#import "MDTracker.h"


@interface MDScreenshotCache()


@property(readwrite,nonatomic,strong) NSURL *baseURL;

@end

@implementation MDScreenshotCache

+(instancetype)sharedCache {
	static MDScreenshotCache *shared = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		shared = [[MDScreenshotCache alloc] init];
	});
	return shared;
}

- (instancetype)init {
	self = [super init];
	if (self) {
		_baseURL = [MDTracker standardDatabaseURL];
	}
	return self;
}


-(BOOL)hasScreenshot:(NSString *)viewIdentifier {
	NSString *filename = [NSString stringWithFormat:@"%@.png",viewIdentifier];
	return [[NSFileManager defaultManager] fileExistsAtPath:[NSURL URLWithString:filename relativeToURL:_baseURL].path];
}

-(UIImage *)screenshot:(NSString *)viewIdentifier {
	NSString *filename = [NSString stringWithFormat:@"%@.png",viewIdentifier];
	NSURL *url = [NSURL URLWithString:filename relativeToURL:_baseURL];
	
	UIImage *image = [[UIImage alloc] initWithContentsOfFile:url.path];
	return image;
}

-(void)save:(UIView *)view asScreenshot:(NSString *)viewIdentifier {
	NSString *filename = [NSString stringWithFormat:@"%@.png",viewIdentifier];
	NSURL *url = [NSURL URLWithString:filename relativeToURL:_baseURL];
	
	UIGraphicsBeginImageContextWithOptions(view.bounds.size, NO, [UIScreen mainScreen].scale);
	
	[view drawViewHierarchyInRect:view.bounds afterScreenUpdates:YES];
	
	
	UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
	UIGraphicsEndImageContext();
	
	[UIImagePNGRepresentation(image) writeToURL:url atomically:YES];
}

@end
