//
//  MDSegueViewController.h
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MDFunnel;

@interface MDSegueViewController : UIViewController

@property(readwrite,nonatomic,strong) NSString *initialViewController;

@property(readwrite,nonatomic,weak) MDFunnel *funnel;
@property(readwrite,nonatomic,strong) NSString *currentFilter;

@property(readwrite,nonatomic) BOOL active;

@end
