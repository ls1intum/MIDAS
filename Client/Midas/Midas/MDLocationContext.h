//
//  MDLocationContext.h
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContext.h"

@class CLLocation;

@interface MDLocationContext : MDContext

@property(readonly,nonatomic,strong) CLLocation *location;

@end
