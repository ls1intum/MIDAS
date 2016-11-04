//
//  MDUploader.m
//  Midas
//
//  Created by Thomas Günzel on 06/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDUploader.h"

#import "GCDAsyncSocket.h"
#import "MDSettings.h"

#import <compression.h>

@interface MDUploader()<GCDAsyncSocketDelegate>

@property(readwrite,nonatomic,strong) GCDAsyncSocket *socket;
@property(readwrite,nonatomic,strong) dispatch_queue_t queue;
@property(readwrite,nonatomic,strong) NSMutableSet *filesToUpload;
@property(readwrite,nonatomic,strong) NSString *currentFile;

@end

@implementation MDUploader

-(instancetype)init {
	self = [super init];
	if (self) {
		_filesToUpload = [[NSMutableSet alloc] init];
		_queue = dispatch_queue_create("de.tum.in.www1.midas-bruegge.uploader", DISPATCH_QUEUE_SERIAL);
		_socket = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:_queue];
	}
	return self;
}

-(void)addFileToUpload:(NSString *)file {
	dispatch_async(_queue, ^{
		[_filesToUpload addObject:file];
		[self uploadNext];
	});
}

-(void)uploadNext {
	if(_currentFile != nil) {
		return;
	}
	NSLog(@"[UPLOAD] Queue: %@",_filesToUpload);
	if([_filesToUpload count] == 0) {
		if([_socket isConnected] == YES) {
			[_socket disconnectAfterReadingAndWriting];
		}
		return;
	}
	
	MDSettings *settings = [MDSettings settingsFromBundledPlist];
	
	if([_socket isConnected] == NO) {
		NSError *error = nil;
		if([_socket connectToHost:settings.uploadHost onPort:settings.uploadPort error:&error] == NO) {
			NSLog(@"Could not connect to host: %@",error);
			return;
		}
	}
	
	NSString *next = [_filesToUpload anyObject];
	_currentFile = next;
	
	NSData *compressedData = [self compress:next];
	if(compressedData == nil) {
		[self removeAndContinue];
		return;
	}
	NSData *headerData = [self createHeaderForLength:(uint32_t)[compressedData length]];
	
	NSMutableData *data = [[NSMutableData alloc] init];
	[data appendData:headerData];
	[data appendData:compressedData];
	
	[_socket writeData:data withTimeout:(5.0 * 60.0) tag:1];
}




-(NSData*)compress:(NSString*)path {
	NSData *data = [NSData dataWithContentsOfFile:path];
	if(data == nil) {
		return nil;
	}
	
	const uint8_t *src = [data bytes];
	size_t src_size = [data length];
	uint8_t *dst = malloc(src_size);
	size_t dst_capacity = src_size;
	compression_algorithm algorithm = COMPRESSION_ZLIB;
	
	size_t dst_size = compression_encode_buffer(dst, dst_capacity, src, src_size, NULL, algorithm);
	
	NSLog(@"Compressed to %lu bytes\n",dst_size);
	if(dst_size == 0) {
		return 0;
	}
	
	NSData *outData = [[NSData alloc] initWithBytesNoCopy:dst length:dst_size freeWhenDone:YES];
	
	return outData;
}

-(NSData*)createHeaderForLength:(uint32_t)length {
	size_t header_size = (sizeof(uint8_t) * 2) + sizeof(uint32_t);
	uint8_t *header = (uint8_t*)malloc(header_size);
	
	header[0] = 'M';
	header[1] = 0x01;
	
	memcpy(&header[2], &length, sizeof(uint32_t));
		
	NSData *headerData = [[NSData alloc] initWithBytesNoCopy:header length:header_size freeWhenDone:YES];
	
	return headerData;
}

-(void)socket:(GCDAsyncSocket *)sock didConnectToHost:(NSString *)host port:(uint16_t)port {
	NSLog(@"Connected");
	[self uploadNext];
}

-(void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
	NSLog(@"Disconnected %@",err);
}

-(void)socket:(GCDAsyncSocket *)sock didWriteDataWithTag:(long)tag {
	NSLog(@"[UPLOAD] Done with %@",_currentFile);
	[self removeAndContinue];
}

-(void)removeAndContinue {
	[[NSFileManager defaultManager] removeItemAtPath:_currentFile error:nil];
	[_filesToUpload removeObject:_currentFile];
	_currentFile = nil;
	[self uploadNext];
}

@end
