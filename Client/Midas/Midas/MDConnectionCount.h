//
//  MDSegueConnection.h
//  Midas
//
//  Created by Thomas Günzel on 17/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MDConnectionCount : NSObject

@property(readwrite,nonatomic) NSUInteger count;
@property(readwrite,nonatomic) NSUInteger all;

@end
