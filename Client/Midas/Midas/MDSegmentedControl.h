//
//  MDSegmentedControl.h
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "MDSegmentedControlItem.h"

@class MDSegmentedControl;

@protocol MDSegmentedControlDelegate <NSObject>

-(void)segmentedControl:(MDSegmentedControl*)control didSelectItem:(MDSegmentedControlItem*)item atIndex:(NSUInteger)itemIndex;

@end

@interface MDSegmentedControl : UIView

@property(readwrite,nonatomic) NSUInteger activeItemIndex;
@property(readwrite,nonatomic,strong) NSArray<MDSegmentedControlItem*> *rootItems;

@property(readwrite,nonatomic,weak) id<MDSegmentedControlDelegate> delegate;

@end
