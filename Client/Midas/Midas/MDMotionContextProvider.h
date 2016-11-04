//
//  MDMotionContextProvider.h
//  Midas
//
//  Created by Thomas Günzel on 17/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContextProvider.h"

@class CMMotionActivity;
@class CMDeviceMotion;

@interface MDMotionContextProvider : MDContextProvider

@property(readonly,atomic,strong) CMMotionActivity *lastActivity;
@property(readonly,atomic,strong) CMDeviceMotion *lastMotion;

@end
