//
//  MDTimeContext.h
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContext.h"

@interface MDTimeContext : MDContext

@property(readonly,nonatomic,strong) NSLocale *locale;

@property(readonly,nonatomic) NSTimeInterval timestamp;

@property(readonly,nonatomic,strong) NSString *timezone;
@property(readonly,nonatomic) NSInteger timezoneOffset;

@end
