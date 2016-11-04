//
//  MDDeviceContext.h
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDContext.h"

@import UIKit;

@interface MDDeviceContext : MDContext

@property(readonly,nonatomic,strong) NSString *model;
@property(readonly,nonatomic,strong) NSString *systemName;
@property(readonly,nonatomic,strong) NSString *systemVersion;
@property(readonly,nonatomic,strong) NSString *appVersion;

@property(readonly,nonatomic) UIUserInterfaceIdiom userInterfaceIdiom;

@end
