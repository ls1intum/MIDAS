//
//  MDMotionContext.h
//  Midas
//
//  Created by Thomas Günzel on 17/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContext.h"

@class CMDeviceMotion;

@interface MDMotionContext : MDContext

@property(readonly,nonatomic,strong) CMDeviceMotion *motion;

@end
