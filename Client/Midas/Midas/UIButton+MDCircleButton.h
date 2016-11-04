//
//  UIButton+MDCircleButton.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface UIButton (MDCircleButton)

+(instancetype)circleButtonWithTitle:(NSString*)title image:(UIImage*)image;

- (void)centerVerticallyWithPadding:(float)padding;
- (void)centerVertically;

- (void)setIncluded:(BOOL)included;
- (BOOL)isIncluded;

@end
