//
//  MPCoding.h
//  MessagePack
//
//  Created by Thomas Günzel on 21/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol MPCoding <NSCoding,NSObject>

-(instancetype)initWithMsgpackDictionary:(NSDictionary*)dictionary;

-(NSUInteger)encodeKeyCount;

@optional
-(NSString*)messagePackType;

@end
