//
//  MDEncoder.h
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import Foundation;

@protocol MDCoding;

@interface MDEncoder : NSObject

-(instancetype)initWithPath:(NSString*)path;

-(void)encodeObject:(id<MDCoding>)object;

-(void)writeData:(uint8_t*)data ofLength:(size_t)length;

-(void)flushActions;
-(void)queueAction:(id<MDCoding>)action;

@property(readonly,nonatomic,strong) NSString *path;
@end
