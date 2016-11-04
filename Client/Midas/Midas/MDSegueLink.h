//
//  MDSegueLink.h
//  Midas
//
//  Created by Thomas Günzel on 17/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@class MDSeguePreview;

@interface MDSegueLink : UIView

-(instancetype)initFrom:(MDSeguePreview*)from to:(MDSeguePreview*)to;


@property(readwrite,nonatomic) CGFloat percentage;

@end
