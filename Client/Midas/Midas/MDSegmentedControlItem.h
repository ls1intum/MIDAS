//
//  MDSegmentedControlItem.h
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDSegmentedControlItem : UIView

-(instancetype)initWithTitle:(NSString*)title color:(UIColor*)color;

@property(readwrite,nonatomic,strong) NSString *title;
@property(readwrite,nonatomic,strong) UIColor *color;
@property(readwrite,nonatomic) BOOL active;

@end
