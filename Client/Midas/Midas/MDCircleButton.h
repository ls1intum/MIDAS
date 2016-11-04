//
//  MDCircleButton.h
//  Midas
//
//  Created by Thomas Günzel on 09/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDCircleButton : UIButton

-(instancetype)initWithTitle:(NSString*)title image:(UIImage*)image;

- (void)centerVerticallyWithPadding:(float)padding;
- (void)centerVertically;

- (void)setIncluded:(BOOL)included;
- (BOOL)isIncluded;

@end
