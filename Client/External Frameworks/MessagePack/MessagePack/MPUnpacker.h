//
//  MPUnpacker.h
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@class MPUnpacker;

@protocol MPUnpackerDelegate <NSObject>

-(void)unpacker:(MPUnpacker*)unpacker didUnpackObject:(id)object;

@end

@interface MPUnpacker : NSObject

-(instancetype)init;

-(void)feed:(NSData*)data;
-(void)feed:(const char *)data length:(size_t)length;

-(void)setClass:(Class)c forType:(NSString*)type;

@property(readwrite,nonatomic,weak) id<MPUnpackerDelegate> delegate;
@property(readwrite,nonatomic,strong) id lastUnpackedObject;
@end
