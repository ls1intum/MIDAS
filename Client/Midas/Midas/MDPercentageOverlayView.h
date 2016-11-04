//
//  MDPercentageOverlayView.h
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MDRecordedAction;

@interface MDPercentageOverlayView : UIView

@property(readwrite,nonatomic,weak) NSArray<MDRecordedAction*> *actions;
@property(readwrite,nonatomic,weak) UIViewController *underlyingViewController;

@end
