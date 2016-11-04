//
//  MDTracker.m
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDTracker.h"

#import "MDRecordedGestures.h"
#import "MDRecordedAction.h"
#import "MDRecordedSegue.h"

#import "MDEncoder.h"
#import "MDUploader.h"

#import "MDDeviceContext.h"

#import "MDLocationContextProvider.h"
#import "MDMotionContextProvider.h"
#import "MDDeviceStateContextProvider.h"

#import "UIViewController+MidasInternal.h"

@interface MDTracker()

@property(readonly,nonatomic,strong) NSURL *databaseURL;
@property(readonly,nonatomic,strong) dispatch_io_t file_io;
@property(readonly,nonatomic,strong) dispatch_queue_t queue;

@property(readonly,nonatomic,strong) MDRecordedGestures *currentRecording;

@property(readonly,nonatomic,strong) MDEncoder *encoder;
@property(readonly,nonatomic,strong) MDUploader *uploader;


@property(readwrite,nonatomic) NSTimeInterval lastViewChangeTimeInterval;

@end

@implementation MDTracker

+(instancetype)sharedTracker {
	static MDTracker *manager = nil;
	static dispatch_once_t onceToken;
	
	dispatch_once(&onceToken, ^{
		manager = [[MDTracker alloc] initWithDatabaseURL:[self standardDatabaseURL]];
	});
	
	return manager;
}

-(instancetype)init {
	return [self initWithDatabaseURL:[[self class] standardDatabaseURL]];
}

-(instancetype)initWithDatabaseURL:(NSURL *)url {
	self = [super init];
	if (self) {
		_enabled = YES;
		_databaseURL = url;
		_queue = dispatch_queue_create([@"Midas.framework" UTF8String], 0);
		__weak typeof(self) weakSelf = self;
		
		dispatch_async(_queue, ^{
			[weakSelf setup];
		});
	}
	return self;
}

-(void)setup {
	_uploader = [[MDUploader alloc] init];
	
	[self startSession];
	
	[[MDLocationContextProvider sharedProvider] increaseNeeded];
	[[MDMotionContextProvider sharedProvider] increaseNeeded];
	[[MDDeviceStateContextProvider sharedProvider] increaseNeeded];
}

-(void)dealloc {
	[[MDLocationContextProvider sharedProvider] decreaseNeeded];
}

#pragma mark - Action Logging

-(void)trackAction:(NSString *)selector onViewController:(NSString *)onViewController sender:(NSString *)senderKeyPath {
	if(!self.enabled) { return; }
	MDRecordedAction *action = [[MDRecordedAction alloc] initWithViewController:onViewController selector:selector senderKey:senderKeyPath];
	[_encoder queueAction:action];
}

-(void)trackSegue:(UIStoryboardSegue *)segue sender:(NSString*)senderKey {
	if(!self.enabled) { return; }
	NSString *src = [segue.sourceViewController md_identifier];
	NSString *dst = [segue.destinationViewController md_identifier];
	
	
	MDRecordedSegue *s = [[MDRecordedSegue alloc] initWithSegueName:segue.identifier sourceViewController:src destinationViewController:dst senderKey:senderKey];
	[_encoder queueAction:s];
}

#pragma mark - Touch Logging


-(void)logEvent:(UIEvent *)event fromWindow:(UIWindow *)window {
	if(!self.enabled) { return; }
	if(event.type != UIEventTypeTouches) {
		return;
	}
	if(_currentRecording) {
		//dispatch_async(_queue, ^{
			[_currentRecording logEvent:event];
		//});
	}
}

-(void)setCurrentViewController:(NSString *)currentViewController {
	if([_currentViewController isEqualToString:currentViewController]) {
		return;
	}
	
	if([currentViewController hasPrefix:@"MD"]) {
		currentViewController = nil;
	}
	
	_currentViewController = currentViewController;
	_currentRecording = nil;
	[_encoder flushActions];
	if(_currentViewController != nil) {
		_currentRecording = [[MDRecordedGestures alloc] initWithViewControllerName:_currentViewController encoder:_encoder];
	}
	
	NSLog(@"Changed View Controller to %@ %@",currentViewController,_currentRecording);
}

#pragma mark - Upload

-(void)adviseUpload {
	NSFileManager *fm = [NSFileManager defaultManager];
	for (NSURL *url in [fm enumeratorAtURL:_databaseURL includingPropertiesForKeys:nil options:(NSDirectoryEnumerationSkipsHiddenFiles|NSDirectoryEnumerationSkipsSubdirectoryDescendants) errorHandler:nil]) {
		if([url.pathExtension isEqualToString:@"midas"]) {
			if ([url.lastPathComponent isEqualToString:_encoder.path.lastPathComponent]) {
				continue;
			}
			
			[_uploader addFileToUpload:url.path];
		}
	}
}

-(void)endSession {
	NSString *current = _currentViewController;
	[self setCurrentViewController:nil];
	_currentViewController = current;
	[_encoder flushActions];
	_encoder = nil;
}

-(void)startSession {
	NSString *filename = [NSString stringWithFormat:@"%.0f.midas",[NSDate timeIntervalSinceReferenceDate]];
	NSString *path = [[_databaseURL URLByAppendingPathComponent:filename] path];
	_encoder = [[MDEncoder alloc] initWithPath:path];
	[_encoder encodeObject:[MDDeviceContext currentContext]];
	NSString *current = _currentViewController;
	if(current != nil) {
		_currentViewController = nil;
		[self setCurrentViewController:current];
	}
	NSLog(@"Saving to %@",_encoder.path);
}

#pragma mark - File System Helpers

+ (NSURL*)applicationDataDirectory {
	NSFileManager* sharedFM = [NSFileManager defaultManager];
	NSArray* possibleURLs = [sharedFM URLsForDirectory:NSApplicationSupportDirectory
											 inDomains:NSUserDomainMask];
	NSURL* appSupportDir = nil;
	NSURL* appDirectory = nil;
 
	if ([possibleURLs count] >= 1) {
		// Use the first directory (if multiple are returned)
		appSupportDir = [possibleURLs objectAtIndex:0];
	}
 
	// If a valid app support directory exists, add the
	// app's bundle ID to it to specify the final directory.
	if (appSupportDir) {
		NSString* appBundleID = [[NSBundle mainBundle] bundleIdentifier];
		appDirectory = [appSupportDir URLByAppendingPathComponent:appBundleID isDirectory:YES];
	}
	
	BOOL directory = NO;
	if([sharedFM fileExistsAtPath:appDirectory.path isDirectory:&directory] == NO || directory == NO) {
		NSError *error = nil;
		if([sharedFM createDirectoryAtPath:appDirectory.path withIntermediateDirectories:YES attributes:nil error:&error] == NO) {
			NSLog(@"Error creating app support subdirectory (%@): %@",appSupportDir,error);
		}
		
	}
 
	return appDirectory;
}


+(NSURL*)standardDatabaseURL {
	return [self applicationDataDirectory];
}

@end
