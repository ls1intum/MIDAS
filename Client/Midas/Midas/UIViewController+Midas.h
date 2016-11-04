//
//  UIViewController+Midas.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "MDViewController.h"

@class MDTracker;

@interface UIViewController (Midas)<MDViewController>

-(void)md_track;

-(void)md_simulateSegue:(NSString*)segueName sender:(NSString*)senderKey;

@property(readonly,nonatomic) UIViewController *topmostViewController;
@property(readonly,nonatomic) UIViewController *md_namedViewController;

@end
