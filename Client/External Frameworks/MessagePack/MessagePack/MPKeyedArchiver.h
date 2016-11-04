//
//  MPKeyedArchiver.h
//  MessagePack
//
//  Created by Thomas Günzel on 21/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MPKeyedArchiver : NSCoder

@property(readonly,nonatomic) NSData *data;

@end
