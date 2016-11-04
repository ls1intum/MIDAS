//
//  MDContext.h
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "MDCoding.h"

@interface MDContext : NSObject<MDCoding>

+(instancetype)currentContext;

@end
