//
//  MDLocationContextProvider.h
//  Midas
//
//  Created by Thomas Günzel on 13/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContextProvider.h"

@class CLLocation;

@interface MDLocationContextProvider : MDContextProvider

@property(readonly,nonatomic) BOOL available;

@property(readonly,nonatomic,strong) CLLocation *lastLocation;

@end
